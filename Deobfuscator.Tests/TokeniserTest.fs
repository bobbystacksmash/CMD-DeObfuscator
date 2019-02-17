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
    member this.Tokeniseaa() =

        let LP = LeftParen "("
        let RP = RightParen ")"
        let CA = CondAlways "&"
        let CS = CondSuccess "&&"
        let PI = Pipe        "|"
        let CO = CondOr      "||"
        let QT = Quote       "\""

        let tests = [
            // Special char identification
            ("(", [LP], "Identify left paren")
            (")", [RP], "Identify right paren")
            ("()", [LP; RP], "Identify left and right parens")
            ("&",  [CA], "Identify CondAlways")
            ("&&", [CS], "Identify CondSuccess")
            ("|",  [PI], "Identify Pipe")
            ("||", [CO], "Identify CondOr")

            // Quotes
            ("\"&^()!\"", [QT; Literal "&^()!"; QT], "Special chars within quotes are ignored.")
            ("calc", [Literal "calc"], "Read literals.")
            ("c^alc", [Literal "calc"], "Read escaped literals as literals.")
            ("a && b", [Literal "a"; Delimiter " "; CondSuccess "&&"; Delimiter " "; Literal "b"], "& become &&")

        ]

        tests |> List.iter (fun test ->
            let input, expected, msg = test
            let actual = tokenise input
            printfn "========================="
            printfn "Actual   -> %A" actual
            printfn "Expected -> %A" expected
            printfn "========================="
            Assert.That(actual, Is.EqualTo(expected))
        )

    (*[<Test>]
    member this.CharTagging() =

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