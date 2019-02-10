namespace Deobfuscator.Tests.Expander

open System
open NUnit.Framework
open Deobfuscator.Expander
open Deobfuscator.Expander.ExpanderWithCommandExtensions

[<TestFixture>]
type TestClass () =

    // [echo %COMSPEC%COMSPEC%] -> [C:\Windows\System32\cmd.exeCOMSPEC%]

    [<Test>]
    member this.SimpleVarExpansion() =

        let vars     = Map.empty.Add("COMSPEC", "C:\\Windows\\System32\\cmd.exe")
        let expected = vars.["COMSPEC"]

        Assert.That((expand "%COMSPEC%" vars), Is.EqualTo(expected), "Match upper-case varname")
        Assert.That((expand "%comspec%" vars), Is.EqualTo(expected), "Match lower-case varname")
        Assert.That((expand "%cOmSpEc%" vars), Is.EqualTo(expected), "Match mixed-case varname")

