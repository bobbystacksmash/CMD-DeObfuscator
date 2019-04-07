open System.Text.RegularExpressions
namespace Deobfuscator.For

open Deobfuscator.DomainTypes
open System.Text.RegularExpressions

type ParseKeyword =
    | KeywordOnly of string
    | KeywordValue of string * string


module ForFileParser =

    let (|KeyValue|Unknown|) str =
        let m = Regex.Match(str, "^([^=]+)=(.+)$")
        if m.Success then
            KeyValue (m.Groups.[1].Value, m.Groups.[2].Value)
        else
            Unknown str


    let private splitParsedKeyword keyword =
        let parts = keyword.split [|' '|] |> Array.toList
        match parts with




    let private (|KeywordEol|_|) str =
        let stripped = Regex.Replace("^\s", str, "")
        let m = Regex.Match(stripped, "^eol=(.)", RegexOptions.IgnoreCase)

        if m.Success then





    let parseForFArgs (keywords: string) =
        // The only allowed keywords are:
        //
        //   - eol
        //   - skip
        //   - delims
        //   - tokens
        //   - usebackq
        //
        // Keywords are case-insensitive, however they will cause
        // a syntax error if they contain invalid values.  For example,
        // the `skip' keyword sets the number of lines to skip, however
        // it actually accepts any positive, non-zero integer, and may
        // be encoded in either DECIMAL, HEX, or OCTAL.
        //
        // Another thing to note is any additional (unrecognised) parse
        // keywords will cause a syntax error.  Also, when wishing to set
        // the `delims' value to a literal space, it must appear last in 
        // the string.
        //
        // delims <-- invalid
        // delims= <-- fine
        type SeenParsingKeywords = {
            EOL: bool
            Skip: bool
            Delims: bool
            Tokens: bool
            UseBackq: bool
        }

        let seenMap = {
            EOL = false; Skip = false; Delims = false; Tokens = false; UseBackq = false
        }

        let rec getKeywordPairs strbuf seenMap =
            






        // `delims=` - no error, also no match.
        // `delims= ` - split on space
        // `delims=   ` - doesn't set delim to `   `

        // DEFAULTS
        //   DELIMS=SPC
        //   EOL=;
        //   TOKENS=1
        //   USEBACKQ can be included as many times as needed in the str




type ForLoopParsingKeywords = {
    EOL: string
    Skip: string
    Delims: char list
    Tokens: int list
    WildcardVar: bool
    UseBackq: bool
}



        parts
