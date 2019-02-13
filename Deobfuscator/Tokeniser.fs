namespace Deobfuscator

type private TokeniserState = { IgnoreMetaChars: bool}

type SpecialChar = char
type RegularChar = char


type Token = 

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
        | '\\' -> SpecialChar
        | _    -> RegularChar

    let tokenise (cmdstr: string) =
        let cmdstrSeq = cmdstr.ToString() |> Seq.toList
        printfn "CMDSTR -> %A" cmdstrSeq