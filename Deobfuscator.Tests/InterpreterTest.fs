namespace Deobfuscator.Tests.Interpreter

open System
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Interpreter
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.Interpret() =
        let vars   = Map.empty.Add("COMSPEC", "C:\\Windows\\System32\\cmd.exe")
        let cmdctx = {
            EnvVars = vars
            StdOut = String.Empty
            // TODO: Configure the default CMD environment.
            InputCmd = "echo x & echo y"
        }

        let result = evaluate cmdctx
        printfn "===================="
        printfn "%A" result
        printfn "===================="

        Assert.IsTrue(false)