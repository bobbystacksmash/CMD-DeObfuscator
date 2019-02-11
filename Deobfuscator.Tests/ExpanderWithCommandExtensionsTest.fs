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
    member this.VarNotDefined() =
        let vars = Map.empty
        Assert.That((expand "%NOT_DEFINED%" vars), Is.EqualTo("%NOT_DEFINED%"))
        Assert.That((expand "%NOT_DEFINED:-3,4%" vars), Is.EqualTo("%NOT_DEFINED:-3,4%"))
        Assert.That((expand "%NOT_DEFINED:foo=bar%" vars), Is.EqualTo("%NOT_DEFINED:foo=bar%"))

    [<Test>]
    member this.VarsDefinedTogether() =
        let vars     = Map.empty.Add("FOO", "bar").Add("HELLO", "world")
        let actual   = expand "%FOO%%HELLO%%FOO%%HELLO%" vars
        let expected = "barworldbarworld"
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.VarsDefinedTogetherWithMissingPercent() =
        let vars = Map.empty.Add("FOO", "bar")
        let actual = expand "%FOO%FOO%" vars
        let expected = "barFOO%"
        Assert.That(actual, Is.EqualTo(expected))


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

    [<Test>]
    member this.SubstringExpansion() =

        let vars = Map.empty.Add("FOO", "123456789ABCDEF")
        let varexp   (a, _, _) = a
        let expected (_, b, _) = b
        let message  (_, _, c) = c

        // Tests from: https://ss64.com/nt/syntax-substring.html.
        let tests = [
            ("%FOO:~0,5%", "12345", "Extract only the first 5 chars")
            ("%FOO:~7,5%", "89ABC", "Skip 7 chars and then extract the next 5")
            ("%FOO:~7%", "89ABCDEF", "Skip 7 characters and then extract everything else.")
            ("%FOO:~-7%", "9ABCDEF", "Extract only the last 7 characters.")
            ("%FOO:~0,-7%", "12345678", "Extract everything BUT the last 7 characters.")
            ("%FOO:~7,-5%", "89AB", "Extract between 7 from the front and 5 from the end.")
            ("%FOO:~-7,-5%", "AB", "Extract between 7 from the end and 5 from the end.")
        ]

        for test in tests do
            Assert.That((expand (varexp test) vars), Is.EqualTo(expected test), (message test))
