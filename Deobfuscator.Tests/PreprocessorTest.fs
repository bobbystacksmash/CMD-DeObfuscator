namespace Deobfuscator.Tests

open System
open NUnit.Framework
open Deobfuscator.Preprocessor

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.TestMethodPassing() =
        Assert.True(true)

    [<Test>]
    member this.Preproessor() =
        let cmd = Seq.toList("hello world".ToCharArray())
        let expected = []
        let actual = Parse { IgnoreMeta = true } cmd []
        Assert.That(actual, Is.EqualTo(expected))