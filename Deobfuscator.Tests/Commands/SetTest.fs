namespace Deobfuscator.Tests.Interpreter.Commands.Set

open System
open NUnit.Framework
open Deobfuscator
open Deobfuscator.Interpreter
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.SimpleSet() =

        let tests = [
            ("set foo=bar", "Assign unquoted and without args key/val.")
        ]

        let expectedEnvVars = Map.empty.Add("foo", "bar")

        tests |> List.iter (fun test ->

            let input, desc = test

            let cmdctx = {
                EnvVars = Map.empty
                StdOut = String.Empty
                StdIn  = String.Empty
                Log = []
                InputCmd = input
            }

            match (evaluate cmdctx) with
            | Ok (status, resctx) ->
                printfn "========================="
                printfn "EXPECTED > %A" expectedEnvVars
                printfn "GOT      > %A" resctx.EnvVars
                printfn "========================="
                Assert.That(resctx.EnvVars, Is.EqualTo(expectedEnvVars))

            | Error reason ->
                Assert.Fail("Failed!")
        )

