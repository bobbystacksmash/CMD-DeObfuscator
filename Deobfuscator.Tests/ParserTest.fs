namespace Deobfuscator.Tests.Tokeniser

open System
open System.Text.RegularExpressions
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Tokeniser
//open Deobfuscator.Translator
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.Parse() =

        let foo = tokenise "x=y"
        printfn "======================"
        printfn "%A" foo
        printfn "======================"
        Assert.IsTrue(false)


    [<Test>]
    member this.AstVerify() =

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
            ("echo=foo", [Cmd [Literal "echo" ; Literal "=" ; Literal "foo";]], "Handle delimiters")

            // Quotes & Escapes
            ("c^alc", [Cmd [Literal "calc"]], "Read escaped literals as literals.")
            ("^c^a^l^c", [Cmd [Literal "calc"]], "Read escaped literals as literals.")
            ("\"calc\"", [Cmd [Literal "\"calc\""]], "Identify quotes around literals.")
            ("\"&^()!\"", [Cmd [Literal "\"&^()!\""]], "Special chars within quotes are literals.")
            ("^&", [Cmd [Literal "&"]], "Escape a special char.")
            ("^\"&", [Op CondAlways ; Cmd [Literal "\""]], "Can escape double quotes.")

            // Conditionals & Redirections
            ("a|b", [Op Pipe ; Cmd [Literal "a"] ; Cmd [Literal "b"]], "Identify pipes without delimiters.")
            ("a && b", [Op CondSuccess ; Cmd [Literal "a" ; Literal " "] ; Cmd [Literal " " ; Literal "b"]], "Identify cond-success with delims.")

            // Commands
            (
                "cmd /C \"echo hello\"",
                [Cmd [Literal "cmd" ; Literal " " ; Literal "/C" ; Literal " " ; Literal "\"echo hello\""]],
                "Tokenise a command."
            )
            (
                "echo hello,world",
                [Cmd [Literal "echo" ; Literal " " ; Literal "hello"; Literal "," ; Literal "world"]],
                "Handle delimiters correctly."
            )

            // Conditionals
            (
                "if 1 == 1 echo test",
                [Cmd
                    [
                        Literal "if"
                        Literal " "
                        Literal "1"
                        Literal " "
                        Literal "="
                        Literal "="
                        Literal " "
                        Literal "1"
                        Literal " "
                        Literal "echo"
                        Literal " "
                        Literal "test"
                    ]
                ],
                "Parse a conditional."
            )
        ]

        // TODO: Add checking for parser errors.
        tests |> List.iter (fun test ->
            let input, expected, msg = test
            let actual = tokenise input
            match tokenise input with
            | Ok actual ->
                printfn "========================="
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