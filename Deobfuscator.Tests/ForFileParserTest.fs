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

        let defaultTokenExpr = { Cols = []; UseWildcard = false }

        let defaults = {
            Skip = 0
            EOL = ";"
            Delims = [' '; '\t']
            Tokens = defaultTokenExpr
            UseBackq = false
        }

        // Successful Tests
        // ----------------
        // "eol=; tokens="              OK
        // "tokens=0xa"                 OK
        // "tokens=1"                   OK
        // "tokens=03"                  OK
        // "tokens=1,5*"                OK
        // ""                           OK
        // "eol= delims="               OK
        // "delims=a delims=b"          OK
        // "delims= delims="            OK
        // "delims="                    OK
        // "delims= "                   OK
        let successfulTests = [

            // Skip, decimal.
            ("skip=5",  { defaults with Skip = 5 }, "Interpret a skip value (dec).")
            ("skip=1",  { defaults with Skip = 1 }, "Interpret a skip value (dec).")
            ("skip=03", { defaults with Skip = 3 }, "Interpret a skip value with leading zero (dec).")

            // Skip, hex.
            ("skip=0xF",    { defaults with Skip = 15   }, "Interpret a skip value (hex, uc)")
            ("skip=0xa",    { defaults with Skip = 10   }, "Interpret a skip value (hex, lc)")
            ("skip=0xff",   { defaults with Skip = 255  }, "Interpret a skip value (hex, mixed)")
            ("skip=0x0012", { defaults with Skip = 18   }, "Interpret a skip value (hex, leading zeros)")

            // Skip, oct. (TODO)
            ("skip=017", { defaults with Skip = 15 }, "Interpret a skip value (oct)")

            // Skip, whtiespace
            ("  skip=2",  { defaults with Skip = 2 }, "Interpret a skip value (leading whitespace)")
            ("skip=4  ",  { defaults with Skip = 4 }, "Interpret a skip value (trailing whitespace)")
            ("  skip=6  ",{ defaults with Skip = 6 }, "Interpret a skip value (leading & trailing whitespace)")

            // Skip, Skip (overwriting defaults)
            ("skip=2 skip=3", { defaults with Skip = 3}, "Overwrite an existing Skip value")

            // Skip, with empty eol value.
            ("skip=2 eol=", {defaults with Skip = 2}, "Correctly handle a (trailing) empty EOL value")

            // EOL, with empty skip.
            ("eol= skip=3", {defaults with Skip = 3}, "Correctly handle (leading) empty EOL ")
            ("eol=", defaults, "Ignore empty EOL")
            ("eol= ", {defaults with EOL = " "}, "Set EOL to an empty string when it appears last.")
            ("eol= eol=", defaults, "Ignore two empty EOLs.")
            ("eol=a eol=b", {defaults with EOL = "b"}, "Take the latter of two EOLs")

            // Useback
            ("useback",  {defaults with UseBackq = true}, "Set usebackq when only 'useback' is given" )
            ("usebackq", {defaults with UseBackq = true}, "Set usebackq when only 'usebackq' is given" )

            // Tokens
            ("tokens=*", {defaults with Tokens = { Cols = []; UseWildcard = true}}, "Handle lone wildcard.")
            ("tokens=1", {defaults with Tokens = { Cols = [1]; UseWildcard = false }}, "Handle simple tokens parsing.")
            ("tokens=3*", {defaults with Tokens = { Cols = [3]; UseWildcard = true}}, "Handle num + wildcard.")
            ("tokens=3,*", {defaults with Tokens = { Cols = [3]; UseWildcard = true}}, "Handle num + wildcard separated by comma.")
            ("tokens=1,2,3,4,5,6", {defaults with Tokens = { Cols = [1..6]; UseWildcard = false }}, "Handle large number of columns.")
            ("tokens=1-2,2-3,3,*", {defaults with Tokens = { Cols = [1; 2; 3]; UseWildcard = true } }, "Correctly parse token expr.")
            ("tokens=0x1,0xa,*", {defaults with Tokens = { Cols = [1; 10]; UseWildcard = true}}, "Parse hex tokens.")
            ("tokens=0xa-0xf", {defaults with Tokens = { Cols = [10; 11; 12; 13; 14; 15]; UseWildcard = false } }, "Handle hex ranges")
            ("tokens=011", {defaults with Tokens = { Cols = [9]; UseWildcard = false}}, "Handle literal octal.")
            ("tokens=017", {defaults with Tokens = { Cols = [15]; UseWildcard = false}}, "Handle octal numbers.")
            ("tokens=015-017*", {defaults with Tokens = { Cols = [13; 14; 15]; UseWildcard = false}}, "Handle octal ranges + wildcard")
            ("tokens=1-2 tokens=2-4", {defaults with Tokens = { Cols = [2..4]; UseWildcard = false}}, "Latter 'tokens=' overwrites former")
            ("tokens= ", defaults, "Allow empty tokens keyword when at the end of the expr.")

            //
            // Delims
            //
            ("delims=,", {defaults with Delims = [',']}, "Set delimiter correctly to a comma.")
            ("delims=abc", {defaults with Delims = ['a'; 'b'; 'c';]}, "Set delimiter to be multiple chars.")

            //
            // Mixed-keyword tests
            //
            (
                "tokens=1 eol=; useback",
                {defaults with Tokens = { Cols = [1]; UseWildcard = false}; EOL = ";"; UseBackq = true},
                "Correctly parse multiple keywords in the same expr."
            )
        ]

        successfulTests |> List.iter (fun test ->
            let input, expected, msg = test
            let output = parseForFArgs input

            match output with
            | Ok output ->
                Assert.That(output, Is.EqualTo(expected), msg)

            | Error reason ->
                printfn "========================="
                printfn "Input    -> [%s]" input
                printfn "Output   -> %A"   output
                printfn "Expected -> %A"   expected
                printfn "Msg      -> %s"   msg
                printfn "========================="
                Assert.Fail("FAILED: " + msg)
        )

    [<Test>]
    member this.ParseForFErrors() =

        // Error Tests
        // -----------
        // "tokens= eol=;"              [ eol=;"] was unexpected at this time.
        // "skip=a"                     [a"] was unexpected at this time.
        // "skip=0"                     ["] was unexpected at this time.
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
            //("skip=a", "Should fail to parse a `skip' keyword when the value is not numeric.")
            ("tokens=0x00", "Should not allow (hex) zero values to be set in tokens keyword.")
            ("tokens=0",    "Should not allow (dec) zero values to be set in tokens keyword.")
            ("tokens=00",   "Should not allow (oct) zero values to be set in tokens keyword.")

            ("tokens= eol=;", "Should not allow the tokens= keyword to be a single space.")
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
