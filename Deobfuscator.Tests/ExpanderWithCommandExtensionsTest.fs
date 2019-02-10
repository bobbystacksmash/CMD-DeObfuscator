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

    [<Test>]
    member this.TrailingColonEdgeCaseExpansion() =

        let vars     = Map.empty.Add("FOO:", "bar")
        let expected = vars.["FOO:"]

        Assert.That((expand "%FOO:%" vars), Is.EqualTo(expected), "Upper-case")
        Assert.That((expand "%foo:%" vars), Is.EqualTo(expected), "Lower-case")
        Assert.That((expand "%FoO:%" vars), Is.EqualTo(expected), "Mixed-case")

        // It should fail to match if we try to use any command extensions.
        let badInput1 = "%FOO::a=b%"
        Assert.That((expand badInput1 vars), Is.EqualTo(badInput1))

        let badInput2 = "%FOO::~3,4%"
        Assert.That((expand badInput2 vars), Is.EqualTo(badInput2))
