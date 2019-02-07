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
        let expected = [
            Literal('h'); Literal('e'); Literal('l'); Literal('l'); Literal('o')
        ]
        let actual = Preprocess cmd
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
        let actual = Preprocess cmd
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    member this.EscapeTests() =
        // Escape all metachars
        let metaChars = [ '(' ; ')'; '%'; '!'; '<'; '>'; '&'; '|'; '^' ]
        let pproc chr = Preprocess ("^" + chr.ToString())

       // TODO
       // ~~~~
       // This command: [echo ^%COMSPEC%] yields: [^C:\Windows\System32\cmd.exe]
       // Need to add some tests to check that '%' is not escaped outside of a string.

        for chr in metaChars do
            Assert.That(pproc chr, Is.EqualTo([Literal(chr)]))

    [<Test>]
    member this.EscapeQuoteSpecificTests() =
        let actual = Preprocess "^\""
        let expected = [Meta('"')]
        Assert.That(actual, Is.EqualTo(expected))