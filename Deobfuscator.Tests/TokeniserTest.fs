namespace Deobfuscator.Tests.Tokeniser

open System
open NUnit.Framework
open Deobfuscator.Tokeniser

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.SimpleTokenising() =

        let input    = "abc"
        let actual   = tokenise input

        let thing = RegularChar('a')

        printfn "This is a thing!"
        Assert.IsTrue(true)

        
        