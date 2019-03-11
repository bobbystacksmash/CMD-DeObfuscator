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
        let vars   = Map.empty.Add("COMSPEC", "C:\\Windows\\System32\\cmd.exe")
        let result = execute vars "\"%COMSPEC%\"/ccalc & echo foo"

        printfn "RES -> %A" result

        Assert.IsTrue(false)