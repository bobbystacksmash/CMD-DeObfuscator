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



        // for /f "delims=a b c" ... -> Error: 'b c' was unexpected at this time
        // for /f "delims=a^ b^ c" ... -> Error: '^b ^c' was unexpected at this time

        let x = parseForFArgs "\"delims=x\""
        printfn ">>> %A" x

        Assert.IsTrue(false)