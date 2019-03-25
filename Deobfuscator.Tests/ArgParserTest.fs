namespace Deobfuscator.Tests.ArgParserTest

open System
open NUnit.Framework
open Deobfuscator.ArgumentParser
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.ArgParserTest() =

        let tests = [
            ("foo bar", ["foo" ; "bar"], "Split on whitespace.")
        ]

        tests |> List.iter (fun test ->
            let input, expected, msg = test

            let actual = argparse input

            printfn "========================="
            printfn "Message  -> %A" msg
            printfn "Input    -> %A" input
            printfn "Actual   -> %A" actual
            printfn "Expected -> %A" expected
            printfn "========================="
            Assert.That(actual, Is.EqualTo(expected))
        )