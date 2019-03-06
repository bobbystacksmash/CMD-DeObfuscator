namespace Deobfuscator.Tests.Interpreter

open System
open NUnit.Framework
open Deobfuscator.Tokeniser
open Deobfuscator.Interpreter
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.Interpret() =
        let tokens = tokenise "foo blah l0l|bar"
        let firstCmd = execute tokens
        Assert.IsTrue(false)