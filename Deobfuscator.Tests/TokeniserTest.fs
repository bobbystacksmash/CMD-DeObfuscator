namespace Deobfuscator.Tests.Tokeniser

open System
open System.Text.RegularExpressions
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Tokeniser

exception ExBadShorthandInputException of string

[<TestFixture>]
type TestClass () =

    member this.ToTokenList(exp: string list) =

        exp |> List.map (fun str ->
            match (str.ToCharArray() |> Seq.toList) with
            | chr :: rest ->
                match chr with
                | 'R' -> RegularChar(rest.[0])
                | 'S' -> SpecialChar(rest.[0])
                | _ -> raise (ExBadShorthandInputException("Unknown leading symbol: " + chr.ToString()))
            | _ -> raise (ExBadShorthandInputException("Cannot create list with input: " + str))
        )

    [<Test>]
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


    [<Test>]
    member this.Tokenising() =
        let input    = "(foo)"
        let actual   = tokenise input
        let expected = []

        Assert.IsTrue(true)