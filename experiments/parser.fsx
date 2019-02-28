open System.Web.UI.WebControls
type Result<'a> =
    | Success of 'a
    | Failure of string


type Column = Column of int
type Length = Length of int
type Location = Location of Column * Length


type Symbol = {
    Value: string
    Location: Location
}

type Token =
    | Literal       of Symbol
    | Quote         of Symbol
    | LeftParen     of Symbol
    | RightParen    of Symbol
    | Delimiter     of Symbol
    | CondAlways    of Symbol
    | CondSuccess   of Symbol
    | CondOr        of Symbol
    | Pipe          of Symbol
    | LeftRedirect  of Symbol
    | RightRedirect of Symbol
    | Escape        of Symbol

    static member CanConcat =
        function
            | Pipe _
            | Literal _
            | Delimiter _
            | CondAlways _ -> true
            | _ -> false


type LookaheadToken =
    | Lookahead of Token * Token list
    | NoMoreTokens


type JoinedToken =
    | Joined of Token
    | NotJoined of Token * Token


type TokenReadMode =
    | MatchSpecial
    | IgnoreSpecial


type ParserState = {
    ReadMode: TokenReadMode
    Escape: bool
    Tokens: Token list
    Input: Symbol list
}



let (|LPAREN|_|) sym =
    if sym.Value = "(" then Some(LeftParen sym) else None

let (|RPAREN|_|) sym =
    if sym.Value = ")" then Some(RightParen sym) else None

let (|PIPE|_|) sym =
    if sym.Value = "|" then Some(Pipe sym) else None

let (|CONDALWAYS|_|) sym =
    if sym.Value = "&" then Some(CondAlways sym) else None

let (|LREDIRECT|_|) sym =
    if sym.Value = "<" then Some(LeftRedirect sym) else None

let (|RREDIRECT|_|) sym =
    if sym.Value = ">" then Some(RightRedirect sym) else None

let (|ESCAPE|_|) sym =
    if sym.Value = "^" then Some(Escape sym) else None

let (|QUOTE|_|) sym =
    if sym.Value = "^" then Some(Quote sym) else None

let (|AMPERSAND|_|) sym =
    if sym.Value = "&" then Some(CondAlways sym) else None

let (|DELIMITER|_|) sym =
    match sym.Value with
    | ","
    | "="
    | " "
    | ";" -> Some(Delimiter sym)
    | _   -> None

let pushToken pstate tok rest =
    {
        pstate with
            Tokens = (tok :: pstate.Tokens);
            Input  = rest
    }

let toggleEscape pState =
    {pState with Escape = (not pState.Escape)}

let escapeOff pState =
    {pState with Escape = false}

let escapeOn pState =
    {pState with Escape = true}

let lookahead (tokens: Token list) =
    if tokens.Length = 0 then NoMoreTokens
    else
        Lookahead(tokens.Head, tokens.Tail)

let catTokens (tokens: Token list) =

    let sameTypeTokens (t0: Token) (t1: Token) =
        t0.GetType() = t1.GetType()

    let canConcat t0 t1 =
        sameTypeTokens t0 t1 && Token.CanConcat(t0)


    // Contiguous tokens that are of the correct type can be joined together.
    // The goal is to build-up tokens which contain as many characters as possible.
    let rec concatenate toks accum =

        match tokens with
        | tok :: rest ->
            match lookahead toks with
            | Lookahead (lahTok, lahRest) when canConcat tok lahTok ->
                concatenate lahRest (// TODO: Add '+' to Token operator for easier joining of tokens)
    concatenate tokens []


let tokenise symbols =

    let rec symbolsToTokens pState =
        match pState.Input with
        | head :: rest ->
            match head with
            | _ when pState.Escape ->
                // The 'escape' flag is set -- ignore any special meaning associated with
                // the current char, and save as a literal, resetting the escape flag.
                symbolsToTokens (pushToken pState (Literal head) rest)

            | ESCAPE esc when pState.ReadMode = MatchSpecial ->
                // We don't push on the '^' (escape) symbol.
                symbolsToTokens {pState with Input = rest; Escape = true}

            | QUOTE qt when pState.ReadMode = MatchSpecial ->
                // A quote toggles the matching of special chars.  The default state is to
                // MATCH special chars.  After the first QUOTE we IGNORE special chars.  This
                // mode flips each time a QUOTE is seen.
                symbolsToTokens ((pushToken pState qt rest) |> escapeOff)

            | QUOTE qt ->
                symbolsToTokens (pushToken pState qt rest)

            | _ when pState.ReadMode = IgnoreSpecial ->
                // We're ignoring special chars
                symbolsToTokens (pushToken pState (Literal head) rest)

            | LPAREN    x
            | RPAREN    x
            | LREDIRECT x
            | RREDIRECT x
            | DELIMITER x
            | AMPERSAND x
            | PIPE      x ->
                symbolsToTokens (pushToken pState x rest)

            | _ ->
                symbolsToTokens (pushToken pState (Literal head) rest)

        | _ -> pState.Tokens |> List.rev


    let pstate = {
        ReadMode = MatchSpecial
        Escape   = false
        Tokens   = []
        Input    = symbols
    }
    symbolsToTokens pstate


let toSymbol ch loc =
    {
        Value = ch.ToString()
        Location = Location (Column loc, Length 1)
    }


let toSymbolList (str: string) =
    let chars = List.ofSeq str
    let locs  = [0..str.Length]
    (Seq.map2 toSymbol chars locs) |> Seq.toList


printfn ">>>>>>>>>>>>> %A" (toSymbolList "foo^&" |> tokenise)

