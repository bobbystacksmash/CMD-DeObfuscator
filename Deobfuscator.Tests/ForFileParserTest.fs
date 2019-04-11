namespace Deobfuscator.Tests.StatementMatcherTest

open NUnit.Framework
open Deobfuscator
open Deobfuscator.DomainTypes
open Deobfuscator.For.ForFileParser
open NUnit.Framework

[<TestFixture>]

type TestClass () =

    [<Test>]
    member this.ParseForFSuccess() =

        let defaults = {
            Skip = 0
            EOL = ";"
            Delims = " "
            Tokens = ""
            UseBackq = false
        }

        // Successful Tests
        // ----------------
        // "skip=1 eol="                OK
        // "eol= skip=1"                OK
        // "eol=; tokens="              OK
        // "tokens=1,2,3,4,5,6,7"       OK
        // "tokens=0xa"                 OK
        // "tokens=1"                   OK
        // "tokens=03"                  OK
        // "tokens=1,5*"                OK
        // ""                           OK
        // "skip=0xa"                   OK
        // "skip=07"                    OK
        // "skip=1 skip=2"              OK
        // "eol= delims="               OK
        // "eol="                       OK
        // "eol= "                      OK
        // "eol= eol="                  OK
        // "eol=a eol=b"                OK
        // "delims=a delims=b"          OK
        // "delims= delims="            OK
        // "delims="                    OK
        // "delims= "                   OK
        // "useback"                    OK
        let successfulTests = [
            ("skip=5", { defaults with Skip = 5 }, "Interpret a skip value in decimal.")
        ]

        successfulTests |> List.iter (fun test ->
            let input, expected, msg = test
            let output = parseForFArgs input
            printfn "========================="
            printfn "Input  -> [%s]" input
            printfn "Output -> %A"   output
            printfn "Msg    -> %s"   msg
            printfn "========================="
            match parseForFArgs input with
            | Ok output ->
                Assert.That(output, Is.EqualTo(expected), msg)

            | Error reason ->
                Assert.Fail(sprintf "Expected input '%A' to equal %A." input expected)
        )

    [<Test>]
    member this.ParseForFErros() =

        // Error Tests
        // -----------
        // "tokens= eol=;"              [ eol=;"] was unexpected at this time.
        // "skip=a"                     [a"] was unexpected at this time.
        //  "eol=abc"                    [bc"]  was unexpected at this time.
        // "eol"                        [eol"] was unexpected at this time.
        // "eol=delims="                [elims="] was unexpected at this time.
        // "delims=a b c"               [b c"] was unexpected at this time.
        // "tokens=a"                   [a"] was unexpected at this time.
        // "tokens=0a"                  [a"] was unexpected at this time.
        // "tokens=0,3"                 [,3"] was unexpected at this time.
        // "tokens=0"                   ["] was unexpected at this time.
        // "tokens=1,0"                 ["] was unexpected at this time.
        //
        let failingTests = [
            ("skip=a", "Should fail to parse a `skip' keyword when the value is not numeric.")
        ]

        failingTests |> List.iter (fun test ->
            let input, msg = test
            let output = parseForFArgs input

            printfn "========================="
            printfn "Input  -> [%s]" input
            printfn "Output -> %A"   output
            printfn "Msg    -> %s"   msg
            printfn "========================="
            match output with
            | Ok output ->
                Assert.Fail("Input string was expected to fail with a reason, instead succeeded.")
            | Error reason ->
                Assert.Pass(sprintf "Failed to parse '%s' - reason: %A" input reason)
        )
