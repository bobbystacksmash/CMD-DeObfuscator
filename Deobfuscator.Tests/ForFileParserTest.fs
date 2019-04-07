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

        // for /f "delims=a b c" ... -> Error: 'b c' was unexpected at this time
        // for /f "delims=a^ b^ c" ... -> Error: '^b ^c' was unexpected at this time

        let x = parseForFArgs "\"delims=x\""
        printfn ">>> %A" x

        Assert.IsTrue(false)