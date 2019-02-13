namespace Deobfuscator

type private CharMatcher =
    | MatchingSpecialChars
    | IgnoringSpecialChars

type private ReaderState = {
    Escape: bool
    Mode: CharMatcher
}

type TokenChar =
    | EscapeChar  of char
    | SpecialChar of char
    | RegularChar of char

module Tokeniser =

    let (|SpecialChar|RegularChar|EscapeChar|) chr =
        match chr with
        | ' '
        | '"'
        | '('
        | ')'
        | '!'
        | '<'
        | '>'
        | '&'
        | '|'
        | '\\' -> SpecialChar chr
        | '^'  -> EscapeChar  chr
        | _    -> RegularChar chr


    let rec private tokeniser (cmdstr: char list) (ctx: ReaderState) (col: TokenChar list) =
        match cmdstr with
        | chr :: rest ->
            match chr with
            | EscapeChar escape ->
                tokeniser rest { ctx with Escape = true } col

            | _ when ctx.Escape ->
                tokeniser rest { ctx with Escape = false} (List.append col [RegularChar(chr)])

            | SpecialChar special ->
                tokeniser rest ctx (List.append col [SpecialChar(chr)])

            | RegularChar regular ->
                tokeniser rest ctx (List.append col [RegularChar(chr)])
        | _ ->
            col


    let tokenise (cmdstr: string) =
        let cmdstrSeq = cmdstr.ToString() |> Seq.toList
        tokeniser cmdstrSeq { Mode = MatchingSpecialChars; Escape = false } []

