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
                | 'E' -> EscapeChar(rest.[0])
                | _ -> raise (ExBadShorthandInputException("Unknown leading symbol: " + chr.ToString()))
            | _ -> raise (ExBadShorthandInputException("Cannot create list with input: " + str))
        )

    [<Test>]
    member this.StandardCharTagging() =

        let tests = [
            ("calc.exe", ["Rc" ; "Ra"; "Rl" ; "Rc" ; "R." ; "Re" ; "Rx" ; "Re"], "Regular char tokenising.")
            ("c^alc", ["Rc" ; "Ra" ; "Rl" ; "Rc"], "Escape tokens are dropped.")
            ("&", ["S&"], "Identify a known special char.")
            ("^&", ["R&"], "Escape a known special char.")
            ("^^", ["R^"], "Escape the escape char.")
            ("^\"", ["S\""], "Cannot escape a double quote.")
            ("^\"ab^\"", ["S\"" ; "Ra" ; "Rb" ; "R^"; "S\""], "Cannot escape an opening or closing double quote.")
            ("(foo)", ["S(" ; "Rf" ; "Ro" ; "Ro" ; "S)"], "Identify parens.")
            (
                "\"!()|?&%><\"",
                ["S\"" ; "R!" ; "R(" ; "R)" ; "R|" ; "R?" ; "R&" ; "R%" ; "R>" ; "R<" ; "S\""],
                "Quotes disable special char identification."
            )
            ("ab\"\"cd", ["Ra" ; "Rb" ; "S\"" ; "S\"" ; "Rc" ; "Rd"], "Empty dquote resets special char matcher.")
            ("\"\"\"", ["S\"" ; "S\"" ; "S\""], "Handling lines of quotes.")
        ]

        tests |> List.iter (fun test ->
            let input, expected, msg = test
            let expectedTokens = this.ToTokenList expected
            let actual = tag input

            printfn "%A" actual

            Assert.That(actual, Is.EqualTo(expectedTokens), msg)
        )
