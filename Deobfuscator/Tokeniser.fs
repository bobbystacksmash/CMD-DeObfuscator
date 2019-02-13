namespace Deobfuscator

type private CharMatcher =
    | MatchingSpecialChars
    | IgnoringSpecialChars

type private ReaderState = {
    Mode: CharMatcher
}

type TokenChar =
    | SpecialChar of char
    | RegularChar of char

module Tokeniser =

    let (|SpecialChar|RegularChar|) chr =
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
        | '^'
        | '\\' -> SpecialChar chr
        | _    -> RegularChar chr


    let rec private tokeniser (cmdstr: char list) (ctx: ReaderState) (col: TokenChar list) =
        match cmdstr with
        | chr :: rest ->
            match chr with
            | SpecialChar special ->
                tokeniser rest ctx (List.append col [SpecialChar(chr)])

            | RegularChar regular ->
                tokeniser rest ctx (List.append col [RegularChar(chr)])
        | _ ->
            col


    let tokenise (cmdstr: string) =
        let cmdstrSeq = cmdstr.ToString() |> Seq.toList
        tokeniser cmdstrSeq { Mode = MatchingSpecialChars } []

