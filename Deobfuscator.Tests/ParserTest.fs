namespace Deobfuscator.Tests.ParserTest

open System
open System.Text.RegularExpressions
open NUnit.Framework
open Deobfuscator
open Deobfuscator.DomainTypes
open Deobfuscator.Parser
//open Deobfuscator.Translator
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    (*[<Test>]
    member this.Parse() =

        let foo = parse "x=y"
        printfn "======================"
        printfn "%A" foo
        printfn "======================"
        Assert.IsTrue(false)*)


    [<Test>]
    member this.GenerateParseTree() =

        let loops = [
            (* FOR Files *)
            @"FOR %G IN (Sun Mon Tue Wed Thur Fri Sat) DO echo %G"
            @"FOR %G IN (MyFile.txt) DO copy %G d:\backups\"
            @"FOR %G IN (Myfile.txt SecondFile.txt) DO copy %G d:\backups\"
            @"FOR %G IN (""C:\demo files\file1.txt"" ""C:\demo files\File2.txt"") DO copy %%G d:\backups\"

            (* FOR /R *)
            @"FOR /R %G in (*.LOG) do Echo REN ""%%G"" ""%%~nG.TXT"""
            @"FOR /R C:\temp\ %%G IN (*.bak) do Echo del ""%%G"""

            (* FOR /D *)
            @"for /f ""tokens=*"" %G in ('dir /b /s /a:d ""C:\Work\reports*""') do echo Found %G"

            (* FOR /L*)
            @"FOR /L %G IN (1,1,5) DO echo %G"

            (* FOR /F*)
            // https://ss64.com/nt/for_f.html
            @"FOR /f ""delims="" %G in (files.txt) DO copy ""\\source\folder\%G"" ""H:\destination\%%G"""
            @"FOR /F ""tokens=1,3 delims=,"" %G IN (weather.txt) DO @echo %G %H"
        ]

        printfn "========================="
        printfn "%A" (parse @"FOR %A IN (1 2 3) DO (echo %A) & calc")
        printfn "========================="

        Assert.IsTrue(false)


    [<Test>]
    member this.VerifyParseTree() =

        let LP = Op OpenParen
        let RP = Op CloseParen
        let CA = Op CondAlways
        let CS = Op CondSuccess
        let PI = Op Pipe
        let CO = Op CondOr

        let str2lit str = Literal str

        let tests = [
            // Special char identification
            ("&",  [CA], "Identify CondAlways")
            ("&&", [CS], "Identify CondSuccess")
            ("|",  [PI], "Identify Pipe")
            ("||", [CO], "Identify CondOr")

            // Literals
            ("calc", [(Cmd [Literal "calc"])], "Read literals.")
            ("echo=foo", [Cmd [Literal "echo" ; Delimiter "=" ; Literal "foo";]], "Handle delimiters")

            // Quotes & Escapes
            ("c^alc", [Cmd [Literal "calc"]], "Read escaped literals as literals.")
            ("^c^a^l^c", [Cmd [Literal "calc"]], "Read escaped literals as literals.")
            ("\"calc\"", [Cmd [Literal "\"calc\""]], "Identify quotes around literals.")
            ("\"&^()!\"", [Cmd [Literal "\"&^()!\""]], "Special chars within quotes are literals.")
            ("^&", [Cmd [Literal "&"]], "Escape a special char.")
            ("^\"&", [Op CondAlways ; Cmd [Literal "\""]], "Can escape double quotes.")

            // Conditionals & Redirections
            ("a|b", [Op Pipe ; Cmd [Literal "a"] ; Cmd [Literal "b"]], "Identify pipes without delimiters.")
            ("a && b", [Op CondSuccess ; Cmd [Literal "a" ; Delimiter " "] ; Cmd [Delimiter " " ; Literal "b"]], "Identify cond-success with delims.")

            // Commands
            (
                "cmd /C \"echo hello\"",
                [Cmd [Literal "cmd" ; Delimiter " " ; Literal "/C" ; Delimiter " " ; Literal "\"echo hello\""]],
                "Tokenise a command."
            )
            (
                "echo hello,world",
                [Cmd [Literal "echo" ; Delimiter " " ; Literal "hello"; Delimiter "," ; Literal "world"]],
                "Handle delimiters correctly."
            )

            // Conditionals
            (
                "if 1 == 1 echo test",
                [Cmd
                    [
                        Literal "if"
                        Delimiter " "
                        Literal "1"
                        Delimiter " "
                        Delimiter "="
                        Delimiter "="
                        Delimiter " "
                        Literal "1"
                        Delimiter " "
                        Literal "echo"
                        Delimiter " "
                        Literal "test"
                    ]
                ],
                "Parse a conditional."
            )
        ]

        // TODO: Add checking for parser errors.
        tests |> List.iter (fun test ->
            let input, expected, msg = test
            let actual = parse input
            match parse input with
            | Ok actual ->
                printfn "========================="
                printfn "Name     -> %A" msg
                printfn "Input    -> %A" input
                printfn "Actual   -> %A" actual
                printfn "Expected -> %A" expected
                printfn "========================="
                Assert.That(actual, Is.EqualTo(expected))
            | Error reason ->
                printfn "FAILED WHILE PARSING INPUT:"
                printfn "%A" input
                printfn "EXPECTED: %A" expected
                Assert.Fail(sprintf "Error while parsing expression -> %A" reason)
        )