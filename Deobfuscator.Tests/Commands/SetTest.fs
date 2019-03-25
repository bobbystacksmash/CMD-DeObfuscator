namespace Deobfuscator.Tests.Interpreter.Commands.Set

open System
open NUnit.Framework
open Deobfuscator
open Deobfuscator.DomainTypes
open Deobfuscator.Interpreter
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.SimpleSet() =

        let expectedEnvVars = Map.empty.Add("FOO", "bar")

        let tests = [
            ("set foo=bar", "Assign unquoted and without args key/val.", expectedEnvVars)
            ("set \"foo=bar\"", "Assignment is between double quotes.", expectedEnvVars)
            ("set \"foo=bar", "Assignment has leading double quote.", expectedEnvVars)
            ("set foo=bar\"", "Leave trailing double quote.", Map.empty.Add("FOO", "bar\""))
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
                Assert.That(resctx.EnvVars, Is.EqualTo(expectedVars))

            | Error reason ->
                Assert.Fail("Failed!")
        )

