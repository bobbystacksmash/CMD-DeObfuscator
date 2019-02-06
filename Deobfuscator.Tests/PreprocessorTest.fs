namespace Deobfuscator.Tests

open System
open NUnit.Framework
open Deobfuscator.Preprocessor

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.TestMethodPassing() =
        Assert.True(true)

    [<Test>]
    member this.TestSimpleTokenising() =
        let cmd = "hello"
        let expected = [Literal('h'); Literal('e'); Literal('l'); Literal('l'); Literal('o')]
        let actual = Preprocess (DefaultParserState ()) cmd
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.TestIdentifyMetachars() =
        let cmd = "()!<>&abc"
        let expected = [
            Meta '(' ;
            Meta ')' ;
            Meta '!' ;
            Meta '<' ;
            Meta '>' ;
            Meta '&' ;
            Literal 'a' ;
            Literal 'b' ;
            Literal 'c' ;
        ]
        let actual = Preprocess (DefaultParserState ()) cmd
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.EscapeTests() =
        // Escape /most/ metachars
        let metaChars = [ '(' ; ')'; '!'; '<'; '>'; '&'; '|'; '^' ]
        let pproc chr = Preprocess (DefaultParserState ()) ("^" + chr.ToString())

        for chr in metaChars do
            Assert.That(pproc chr, Is.EqualTo([Literal(chr)]))

    [<Test>]
    member this.TestVarSigilsAreAlwaysMeta() =
        let pproc cmd = Preprocess (DefaultParserState ()) cmd

        // '%' is special, and is therefore always identified as a meta.
        Assert.That( (pproc "^%"),  Is.EqualTo([Meta('%')]))
        Assert.That( (pproc "\"%"), Is.EqualTo([Meta('"'); Meta('%')]))

        let regularComspec = [
            Meta('%')
            Literal('C')
            Literal('o')
            Literal('M')
            Literal('s')
            Literal('P')
            Literal('e')
            Literal('C')
            Meta('%')
        ]

        Assert.That((pproc "%CoMsPeC%"), Is.EqualTo(regularComspec))

        // Default parser state is DelayedExp=FALSE.  Let's just check that
        // '^!' is not treated as meta in this state.
        Assert.That( (pproc "^!"), Is.EqualTo([Literal('!')]))
        Assert.That( (pproc "\"!"), Is.EqualTo([Meta('"'); Literal('!')]))

        let comspec = [
            Literal('!')
            Literal('C')
            Literal('O')
            Literal('M')
            Literal('S')
            Literal('P')
            Literal('E')
            Literal('C')
            Literal('!')
        ]
        let output = (pproc "^!COMSPEC^!")
        Assert.That( (pproc "^!COMSPEC^!"), Is.EqualTo(comspec), output.[8].ToString())

        // '!' is similar, but only when delayed expansion is set.
        let pprocDelayed cmd =
            let delayedExpansionState = {
                (DefaultParserState ()) with DelayedExpansion = true
            }
            Preprocess delayedExpansionState cmd

        Assert.That( (pprocDelayed "^!"),  Is.EqualTo([Meta('!')]))
        Assert.That( (pprocDelayed "\"!"), Is.EqualTo([Meta('"'); Meta('!')]))

    [<Test>]
    member this.EscapeQuoteSpecificTests() =
        let actual = Preprocess (DefaultParserState ()) "^\""
        let expected = [Meta('"')]
        Assert.That(actual, Is.EqualTo(expected), "Cannot escape a double quote.")

    [<Test>]
    member this.ExpandDefinedEnvironmentVariables() =

        let tokens = Preprocess (DefaultParserState ()) "%comspec%"
        let actual = Expand tokens

        Assert.True(true)
        // TODO: [%COMSPEC%]   -> C:\Windows\System32\cmd.exe
        // TODO: ["%COMSPEC%"] -> "C:\Windows\System32.cmd.exe"
        // TODO: [%%COMSPEC%%] -> %C:\Windows\System32\cmd.exe"
        // TODO: ["%%COMSPEC%%"] -> "%C:\Windows\System32\cmd.exe%"
        // TODO: Expansions are only applied once. For example, assuming blah is set:
        //         [echo %%blah%%] -> %comspec%
        //                            ^^^^^^^^^
        //                            Not expanded further.

