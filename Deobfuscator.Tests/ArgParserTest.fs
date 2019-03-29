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
        ]

        tests |> List.iter (fun test ->

            let actual = parseArgs test

            printfn "========================="
            printfn "Input    -> %A" test
            printfn "Expected -> %A" hello
            printfn "Actual   -> %A" actual
            printfn "========================="
            Assert.That(actual, Is.EqualTo(hello))
        )

    [<Test>]
    member this.ArgumentParsing() =
        //
        // All credit to:
        //  http://www.windowsinspired.com/how-a-windows-programs-splits-its-command-line-into-individual-arguments/
        //
        let tests =[
            (
                "\"She said \"you can't do this!\", didn't she?\"",
                ["She said you"; "can't"; "do"; "this!, didn't she?"],
                "Correct groupings of an input argstr."
            )
            ("First Second Third", ["First"; "Second"; "Third"], "Split on whitespace")
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