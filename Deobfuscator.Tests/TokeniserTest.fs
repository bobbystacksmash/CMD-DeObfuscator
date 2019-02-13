namespace Deobfuscator.Tests.Tokeniser

open System
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Tokeniser

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.SimpleTokenising() =

        let input    = "abc"
        let actual   = tokenise input
        let expected = [
            RegularChar('a') ; RegularChar('b') ; RegularChar('c')
        ]

        Assert.That(actual, Is.EqualTo(expected))