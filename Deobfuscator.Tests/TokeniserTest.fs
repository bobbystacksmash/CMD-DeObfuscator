namespace Deobfuscator.Tests.Tokeniser

open System
open System.Text.RegularExpressions
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Tokeniser
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

    (*[<Test>]
    member this.BuildAST() =
        let input = "foo&bar"
        let actual = tokenise input |> buildAST

        let lhs = ((Command [Literal "foo"]), Empty, Empty)
        let rhs = ((Command [Literal "bar"]), Empty, Empty)
        let ast = ((Operator (CondAlways "&"), lhs, rhs))

        Assert.That(actual, Is.EqualTo(ast))

        //let lhs = Command ((Literal "foo"), Empty, Empty)
        //let rhs = Command ((Literal "bar"), Empty, Empty)
        //let ast = Operator ((CondAlways "&"), lhs, rhs)
        //let actual = tokenise input |> buildAST
        //Assert.That(actual, Is.EqualTo(ast))*)

    (*[<Test>]
    member this.CharTagging() =
g
        let tests = [
            ("calc.exe", ["Rc" ; "Ra"; "Rl" ; "Rc" ; "R." ; "Re" ; "Rx" ; "Re"], "Regular char tokenising.")
            ("c^alc", ["Rc" ; "Ra" ; "Rl" ; "Rc"], "Escape tokens are dropped.")
            ("&", ["S&"], "Identify a known special char.")
            ("^&", ["R&"], "Escape a known special char.")
            ("^^", ["R^"], "Escape the escape char.")
            ("^\"", ["R\""], "Can escape a double quote.")
            ("^\"ab^\"", ["R\"" ; "Ra" ; "Rb" ; "R\""], "Can escape an opening or closing double quote.")
            ("(foo)", ["S(" ; "Rf" ; "Ro" ; "Ro" ; "S)"], "Identify parens.")
            (
                "\"!()|?&%><\"",
                ["S\"" ; "R!" ; "R(" ; "R)" ; "R|" ; "R?" ; "R&" ; "R%" ; "R>" ; "R<" ; "S\""],
                "Quotes disable special char identification."
            )
            ("ab\"\"cd", ["Ra" ; "Rb" ; "S\"" ; "S\"" ; "Rc" ; "Rd"], "Empty dquote resets special char matcher.")
            ("\"\"\"", ["S\"" ; "S\"" ; "S\""], "Handling lines of quotes.")
            ("^\"&", ["R\"" ; "S&"], "Can escape double quotes.")
        ]

        tests |> List.iter (fun test ->
            let input, expected, msg = test
            let expectedTags = this.ToTokenList expected
            let actual = tag input

            printfn "EXPECTED -> %A" expectedTags
            printfn "ACTUAL   -> %A" actual
            printfn "------"

            Assert.That(actual, Is.EqualTo(expectedTags), msg)
        )
    *)