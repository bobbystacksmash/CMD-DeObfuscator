namespace Deobfuscator.Tests.StatementMatcherTest

open NUnit.Framework
open Deobfuscator
open Deobfuscator.DomainTypes
open Deobfuscator.Parser
open Deobfuscator.StatementMatcher
open NUnit.Framework


[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.MatchStatements() =

        // for /f "eol=; tokens=2,3* delims=," %i in (myfile.txt) do @echo %i %j %k

        //let parseResult = parse @"FOR /L %A IN (1 2 3) DO echo %A"
        //let ast = parse @"FOR %A IN (1 2 3) DO echo %A"
        //let parseResult = parse @"FOR /f ""eol=; tokens=2,3* delims=,"" %i in (myfile.txt) do @echo %i %j %k"
        //let parseResult = parse @"(FOR /l %A IN (1,1,10) DO (FOR /l %B IN (5,1,10) DO ECHO B %B, A %A))"
        //let parseResult = parse @"(FOR /l %A IN (1,1,10) DO (ECHO %A))"
        let parseResult = parse "calc &"

        printfn "ParseTree --> %A" parseResult
        Assert.Fail("Failed.")

        //let ast = parse @"FOR /F ""tokens=4 delims=,"" %G IN (""deposit,$4500,123.4,12-AUG-09"") DO echo Date paid %G"
        (*match parseResult with
        | Ok astList ->
            tryConvertParseTreeToAst astList
            Assert.IsTrue(false)

        | Error reason ->
            Assert.Fail("Failed.")*)

