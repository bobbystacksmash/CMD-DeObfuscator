namespace Deobfuscator.Tests.Tokeniser

open System
open System.Text.RegularExpressions
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Tokeniser
open Deobfuscator.AbstractSyntaxTree
open NUnit.Framework

exception ExBadShorthandInputException of string


[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.Tokenise() =

        let LP = LeftParen(Symbol "(")
        let RP = RightParen(Symbol ")")
        let CA = CondAlways(Symbol "&")
        let CS = CondSuccess(Symbol "&&")
        let PI = Pipe(Symbol "|")
        let CO = CondOr(Symbol "||")
        let QT = Quote(Symbol "\"")
        let SP = Delimiter(Symbol " ")

        let str2lit str = Literal(Symbol str)

        let tests = [
            // Special char identification
            ("(", [LP], "Identify left paren")
            (")", [RP], "Identify right paren")
            ("()", [LP; RP], "Identify left and right parens")
            ("&",  [CA], "Identify CondAlways")
            ("&&", [CS], "Identify CondSuccess")
            ("|",  [PI], "Identify Pipe")
            ("||", [CO], "Identify CondOr")

            // Literals
            ("calc", [str2lit "calc"], "Read literals.")

            // Quotes & Escapes
            ("c^alc", [str2lit "calc"], "Read escaped literals as literals.")
            ("^c^a^l^c", [str2lit "calc"], "Read escaped literals as literals.")
            ("\"&^()!\"", [QT; str2lit "&^()!"; QT], "Special chars within quotes are ignored.")
            ("^&", [str2lit "&"], "Escape a special char.")
            ("^\"&", [str2lit "\""; CA], "Can escape double quotes.")

            // Conditionals & Redirections
            ("a|b", [str2lit "a"; PI; str2lit "b"], "Identify pipes without delimiters.")
            ("a && b", [str2lit "a"; SP; CondSuccess(Symbol "&&"); SP; str2lit "b"], "& become &&")

            // Keyword Detection
            // ...

            // Commands
            (
                "cmd /C \"echo hello\"",
                [str2lit "cmd"; SP; str2lit "/C"; SP; QT; str2lit "echo hello"; QT],
                "Tokenise a command."
            )
        ]

        tests |> List.iter (fun test ->
            let input, expected, msg = test
            let actual = tokenise input
            printfn "========================="
            printfn "Input    -> %A" input
            printfn "Actual   -> %A" actual
            printfn "Expected -> %A" expected
            printfn "========================="
            Assert.That(actual, Is.EqualTo(expected))
        )