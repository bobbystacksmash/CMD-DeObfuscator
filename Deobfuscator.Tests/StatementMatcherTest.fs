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

        let ast = parse @"FOR %A IN (1 2 3) DO echo %A"
        enrichAst ast

        Assert.IsTrue(false)