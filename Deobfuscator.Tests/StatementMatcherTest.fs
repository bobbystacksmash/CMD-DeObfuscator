namespace Deobfuscator.Tests.StatementMatcherTest

open System
open System.Text.RegularExpressions
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

        //let ast = parse @"FOR /L %A IN (1 2 3) DO echo %A"
        //let ast = parse @"FOR %A IN (1 2 3) DO echo %A"
        //let ast = parse @"FOR /f ""eol=; tokens=2,3* delims=,"" %i in (myfile.txt) do @echo %i %j %k"
        let ast = parse @"FOR /F ""tokens=4 delims=,"" %G IN (""deposit,$4500,123.4,12-AUG-09"") DO echo Date paid %G"
        liftAst ast

        Assert.IsTrue(false)