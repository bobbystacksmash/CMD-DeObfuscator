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
        // April, 2019:
        // This is *REALLY* hacky, and I'm sure there's a smarter way to do exception
        // checking.  Perhaps returning a Result is the wrong pattern -- maybe exceptions
        // are better suited?  Either way, still learning F# so this'll have to do (for now).
        let checkCorrectErrorReturned errName output =
            printfn "Checking ErrName -> %A" errName
            match output with
            | Ok _ -> false
            | Error err ->
                printfn "What type of err? > %A" err
                match err with
                | FeatureNotImplemented -> false
                | KeywordSkipValueIsNotNumeric _ when errName = "KeywordSkipValueIsNotNumeric" -> true
                | KeywordSkipCannotBeZero _ when errName = "KeywordSkipCannotBeZero" -> true
                | KeywordTokensIsInvalid _ when errName = "KeywordTokensIsInvalid" -> true
                | ExpectedParseKeywordValue _ when errName = "ExpectedParseKeywordValue" -> true
                | _ -> false


        // Error Tests
        // -----------
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
            (*("skip=a", "KeywordSkipValueIsNotNumeric", "Should fail to parse a `skip' keyword when the value is not numeric.")
            ("skip=0", "KeywordSkipCannotBeZero", "Should not allow skip to equal zero.")

            ("tokens=0x00-0x01", "KeywordTokensIsInvalid", "Should not allow (hex) zero values to be set in tokens keyword.")
            ("tokens=0", "KeywordTokensIsInvalid", "Should not allow (dec) zero values to be set in tokens keyword.")*)
            ("tokens=00", "KeywordTokensIsInvalid", "Should not allow (oct) zero values to be set in tokens keyword.")

            ("tokens= eol=;", "ExpectedParseKeywordValue", "Should not allow the tokens= keyword to be a single space.")
        ]

        let runTest test =
            let input, expectedErr, msg = test
            let output = parseForFArgs input
            let outbuf = [
                "========================="
                (sprintf "Input  -> [%s]" input)
                (sprintf "Output -> %A"   output)
                (sprintf "Msg    -> %s"   msg)
                "========================="
            ]
            (checkCorrectErrorReturned expectedErr output, outbuf)


        let findFailingTests testResult =
            match testResult with
            | (false, reason) -> true
            | _ -> false


        let results =
            failingTests
            |> List.map runTest
            |> List.filter findFailingTests

        if results.Length = 0 then
            Assert.Pass("All expected errors were thrown.")
        else
            printfn "************"
            printfn "RESULTS > %A" results
            results
            |> List.map snd
            |> List.iter (fun output -> output |> List.iter (fun line -> printfn "%s" line))
            Assert.Fail("Test ran without erroring (this is bad - these tests check errors!)")


