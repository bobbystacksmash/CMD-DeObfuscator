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

        let expectedEnvVars = Map.empty.Add("foo", "bar")

        let tests = [
            ("set foo=bar", "Assign unquoted and without args key/val.", expectedEnvVars)
            ("set \"foo=bar\"", "Assignment is between double quotes.", expectedEnvVars)
        ]


        tests |> List.iter (fun t ->

            let (cmd, msg, expectedVars) = t

            let cmdctx = {
                EnvVars = Map.empty
                StdOut = String.Empty
                StdIn  = String.Empty
                Log = []
                InputCmd = cmd
            }

            match (evaluate cmdctx) with
            | Ok (status, resctx) ->
                printfn "========================="
                printfn "EXPECTED > %A" expectedVars
                printfn "GOT      > %A" resctx.EnvVars
                printfn "========================="
                Assert.That(resctx.EnvVars, Is.EqualTo(expectedEnvVars))

            | Error reason ->
                Assert.Fail("Failed!")
        )

