namespace Deobfuscator.Tests.ArgParserTest

open System
open NUnit.Framework
open Deobfuscator.ArgumentParser
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.FiftyWaysToSayHello() =
        //
        // All credit to:
        //  http://www.windowsinspired.com/50-ways-to-say-hello/
        //
        let hello = ["Hello"]

        let tests = [
            "\"\"\"Hello\"\""
            "\"\"\"Hello\"\"\""
            "\"\"\"Hello\\\"\""

        ]

        tests |> List.iter (fun test ->

            let actual = parseArgs test

            printfn "========================="
            printfn "Input    -> %s" test
            printfn "Expected -> %s" hello.[0]
            printfn "Actual   -> %A" actual
            printfn "========================="
            Assert.That(actual, Is.EqualTo(hello))
        )

    [<Test>]
    member this.ArgumentWithSpaces() =
        //
        // All credit to:
        //  http://www.windowsinspired.com/how-the-argument-with-spaces-examples-are-parsed/
        //
        let expected = ["Argument with spaces"]

        let tests = [
            "\"Argument with spaces\""
            "Argument\" \"with\" \"spaces"
            "\"Argument \"with\" spaces\""
            "Argument\" with sp\"aces"
            "\"Argument with spaces"
            "\"Ar\"g\"um\"e\"n\"t\" w\"it\"h sp\"aces\"\""
        ]

        tests |> List.iter (fun test ->

            let actual = parseArgs test

            printfn "========================="
            printfn "Input    [%s]" test
            printfn "Expected:"
            for exp in expected do
                printfn "  [%s]" exp

            printfn "Actual:"
            for act in actual do
                printfn "  [%s]" act

            printfn "========================="
            Assert.That(actual, Is.EqualTo(expected))
        )

    [<Test>]
    member this.ArgumentParsing() =
        //
        // All credit to:
        //  http://www.windowsinspired.com/how-a-windows-programs-splits-its-command-line-into-individual-arguments/
        //
        let tests =[
            (
                "Test.exe \"First Argument\" Second",
                ["Test.exe"; "First Argument"; "Second"],
                "Correctly generate command line."
            )
            (
                "\"She said \"you can't do this!\", didn't she?\"",
                ["She said you"; "can't"; "do"; "this!, didn't she?"],
                "Correct groupings of an input argstr."
            )
            (
                "First Second Third",
                ["First"; "Second"; "Third"],
                "Split on whitespace"
            )
            (
                "w\"\"script",
                ["wscript"],
                "Handle empty dquotes used to obfuscate command."
            )
        ]

        tests |> List.iter (fun test ->
            let input, expected, msg = test

            let actual = parseArgs input

            printfn "========================="
            printfn "Input   -> %A" input
            printfn "Expected -> %A" expected
            printfn "Actual   -> %A" actual
            printfn "========================="
        )