namespace Deobfuscator.Tests.StatementMatcherTest

open NUnit.Framework
open Deobfuscator
open Deobfuscator.DomainTypes
open Deobfuscator.For.ForFileParser
open NUnit.Framework

[<TestFixture>]

type TestClass () =

    [<Test>]
    member this.ParseForFKeywords() =

        // Tests
        // =====
        //
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
        //
        //
        //
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
        let failingTests = [
            ("skip=a", "Should fail to parse a `skip' keyword when the value is not numeric.")
        ]

        failingTests |> List.iter (fun test ->
            let input, msg = test

            match parseForFArgs input with
            | Ok output ->
                Assert.Fail("Input string was expected to fail with a reason, instead succeeded.")
            | Error reason ->
                printfn "========================="
                printfn "Input      -> [%s]" input
                printfn "Err Reason -> %A"   reason
                printfn "Msg        -> %s"   msg
                printfn "========================="
                Assert.That(reason, Is.EqualTo(FeatureNotImplemented))
        )
