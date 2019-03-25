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
            "\"\"\"Hello\"\""
        ]

        tests |> List.iter (fun test ->

            let actual = argparse test

            printfn "========================="
            printfn "Input    -> %A" test
            printfn "Actual   -> %A" actual
            printfn "Expected -> %A" hello
            printfn "========================="
            Assert.That(actual, Is.EqualTo(hello))
        )