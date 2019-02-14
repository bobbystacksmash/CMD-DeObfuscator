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
    member this.FindReplaceExpansion() =

        let varexp   (a, _, _) = a
        let expected (_, b, _) = b
        let message  (_, _, c) = c

        let vars = Map
                    .empty
                    .Add("ONE",   "AABBCCDDEEFF")
                    .Add("TWO",   "HELLO WORLD")
                    .Add("THREE", "w|s|c|r|i|p|t")
                    .Add("PATH", "C:\\Windows\\System32\\")

        let findReplaceTests = [
            ("%ONE:A=Z%", "ZZBBCCDDEEFF", "Replace all occurrances of a char within the string.")
            ("%ONE:a=z%", "zzBBCCDDEEFF", "Replace all occurrances of a char within the string (case-insensitive).")
            ("%ONE:B=%", "AACCDDEEFF", "Remove all matches from the string when no RHS of replace.")
            ("%ONE:X=Z%", "AABBCCDDEEFF", "Leave string as-is when find cannot be found.")
            ("%ONE:AABBCCDDEEFF=x%", "x", "Replace whole string with new char.")
            ("%ONE:AABBCCDDEEFF=%", "", "Replace whole string - return empty string.")
            ("%TWO: =_%", "HELLO_WORLD", "Replace spaces")
            ("%TWO: =_%", "HELLO_WORLD", "Replace spaces")
            ("%THREE:|=%", "wscript", "Remove regexp metachar '|'")
            ("%PATH:\\=/%", "C:/Windows/System32/", "Replace \\ with /")
        ]
        for test in findReplaceTests do
            Assert.That((expand (varexp test) vars), Is.EqualTo(expected test), (message test))


    [<Test>]
    member this.SubstringExpansion() =

        let vars = Map.empty.Add("FOO", "123456789ABCDEF")
        let varexp   (a, _, _) = a
        let expected (_, b, _) = b
        let message  (_, _, c) = c

        // Tests from: https://ss64.com/nt/syntax-substring.html.
        let tests = [
            ("%FOO:~-0,0%", "", "Do not extract any chars when 0,0")
            ("%FOO:~-0,-0%", "", "Do not extract any chars when -0,-0")
            ("%FOO:~0,-0%", "", "Do not extract any chars when 0,-0")
            ("%FOO:~0,5%", "12345", "Extract only the first 5 chars")
            ("%FOO:~7,5%", "89ABC", "Skip 7 chars and then extract the next 5")
            ("%FOO:~7%", "89ABCDEF", "Skip 7 characters and then extract everything else.")
            ("%FOO:~-7%", "9ABCDEF", "Extract only the last 7 characters.")
            ("%FOO:~0,-7%", "12345678", "Extract everything BUT the last 7 characters.")
            ("%FOO:~7,-5%", "89AB", "Extract between 7 from the front and 5 from the end.")
            ("%FOO:~-7,-5%", "AB", "Extract between 7 from the end and 5 from the end.")
            // Whitespace
            ("%FOO:~      -7,-5%", "AB", "Ignore whitespace, and extract between 7 from the end and 5 from the end.")
            ("%FOO:~-7,      -5%", "AB", "Ignore whitespace, and extract between 7 from the end and 5 from the end.")
            ("%FOO:~  -7,    -5%", "AB", "Ignore whitespace, and extract between 7 from the end and 5 from the end.")
        ]
        for test in tests do
            Assert.That((expand (varexp test) vars), Is.EqualTo(expected test), (message test))

        // Tests for cases where the substrings fall beyond the bounds of the string.
        let failingTests = [
            ("%FOO:~100%", "", "Skip 100 chars, then just return the empty string.")
            ("%FOO:~100,500%", "", "Skip 100 chars, then try to read 500 more - return the empty string.")
            ("%FOO:~0,500%", vars.["FOO"], "Read the first 500 chars - return the whole string")
            ("%FOO:~1,15%", "23456789ABCDEF", "Read 1 char, then fetch remander of value.")
            ("%FOO:~-15%", vars.["FOO"], "Should read the whole string when negative length = strlen.")
            ("%FOO:~-16%", vars.["FOO"], "Should read the whole string when negative length > strlen.")
            ("%FOO:~-26%", vars.["FOO"], "Should read the whole string when negative length > strlen.")
        ]
        for failingTest in failingTests do
            Assert.That((expand (varexp failingTest) vars), Is.EqualTo(expected failingTest), (message failingTest))

        // Hexadecimal substr handling
        let hexadecimalTests = [
            ("%FOO:~0x00,0xf%", "123456789ABCDEF", "Correctly convert hex to dec and return expected.")
            ("%FOO:~0x00,0x0000%", "", "Correctly convert hex to dec and return expected.")
            ("%FOO:~0xa,0xf%", "BCDEF", "Correctly convert hex to dec and return expected.")
            ("%FOO:~-0xb%", "56789ABCDEF", "Correctly convert hex to dec and return expected.")
        ]
        for hexTest in hexadecimalTests do
            Assert.That((expand (varexp hexTest) vars), Is.EqualTo(expected hexTest), (message hexTest))

        // Octal substr handling
        let octalTests = [
            ("%FOO:~0,012%", "123456789A", "Correctly convert oct to dec and return expected.")
            ("%FOO:~0,00%", "", "Correctly convert oct to dec and return expected.")
        ]
        for octTest in octalTests do
            Assert.That((expand (varexp octTest) vars), Is.EqualTo(expected octTest), (message octTest))

        // Should not replace a variable that is not defined.
        Assert.That((expand "%NOT_DEFINED:~3,5%" vars), Is.EqualTo("%NOT_DEFINED:~3,5%"))
