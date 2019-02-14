namespace Deobfuscator

type private TagMatcher =
    | MatchingSpecialChars
    | IgnoringSpecialChars

type private TagReaderState = {
    Escape: bool
    Mode: TagMatcher
}

type TagChar =
    | SpecialChar of char
    | RegularChar of char


type private Token =
    | LPAREN
    | RPAREN
    | DELIMITER


module Tokeniser =

    let (|SpecialChar|QuoteChar|RegularChar|EscapeChar|) chr =
        match chr with
        | ' '
        | '('
        | ')'
        | '!'
        | '<'
        | '>'
        | '&'
        | '|'
        | '\\' -> SpecialChar chr
        | '"'  -> QuoteChar   chr
        | '^'  -> EscapeChar  chr
        | _    -> RegularChar chr


    let rec private tagChars (cmdstr: char list) (ctx: TagReaderState) (col: TagChar list) =
        match cmdstr with
        | chr :: rest ->
            match chr with

            | _ when ctx.Escape ->
                tagChars rest { ctx with Escape = false } (List.append col [RegularChar(chr)])

            | EscapeChar escape ->
                tagChars rest { ctx with Escape = true } col

            | QuoteChar dquote when ctx.Mode = MatchingSpecialChars ->
                tagChars rest { ctx with Escape = false ; Mode = IgnoringSpecialChars; } (List.append col [SpecialChar(chr)])

            | QuoteChar dquote ->
                tagChars rest { ctx with Mode = MatchingSpecialChars } (List.append col [SpecialChar(chr)])

            | _ when ctx.Mode = IgnoringSpecialChars ->
                tagChars rest ctx (List.append col [RegularChar(chr)])

            | SpecialChar special ->
                tagChars rest ctx (List.append col [SpecialChar(chr)])

            | RegularChar regular ->
                tagChars rest ctx (List.append col [RegularChar(chr)])
        | _ ->
            col

    let rec private tokeniseTags (tags: TagChar list) =
        12

    /// <summary>Tagging is the first part of the tokenising process.  As the tagger
    /// scans the input text, it classifies tokens in to two types: SpecialChars, and
    /// RegularChars.  Once tagged, we feed these tokens in to the tokeniser, which will
    /// take is from 'SpecialChar('&&')' to the token 'CALL_UPON_SUCCESS'.</summary>
    let tag (cmdstr: string) =
        let cmdstrSeq = cmdstr.ToString() |> Seq.toList
        tagChars cmdstrSeq { Mode = MatchingSpecialChars; Escape = false } []


    let tokenise (cmdstr: string) =
        tag cmdstr |> tokeniseTags