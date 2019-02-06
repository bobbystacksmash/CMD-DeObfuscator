// CMD De-Obfuscator
// =================

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

type TokeniserState = { IgnoreSpecialChars: bool}

let cmd = "\"Ar\"g\"u^m\"e\"n\"t\" W\"it\"h Sp\"aces\"\""

// The parser runs in two modes:
//
//   1. IgnoreSpecialChars: true
//      When in this mode, the parser will treat

let rec QuoteReader (chars: char list) state col =
    let parserState = state.IgnoreSpecialChars
    match (Seq.toList chars) with
    | head :: rest ->
        match head with
        | SpecialChar -> QuoteReader rest { IgnoreSpecialChars = not parserState } col
        | RegularChar -> QuoteReader rest state (List.append col [head])
    | [] -> col
        
let cmdStr = Seq.toList (cmd.ToCharArray())
let output = QuoteReader cmdStr { IgnoreSpecialChars = false } []
printfn "OUT -----> %A" output
