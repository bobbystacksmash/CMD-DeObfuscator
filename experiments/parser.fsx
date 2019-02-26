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
    | CondAlways    of Symbol
    | CondSuccess   of Symbol
    | CondOr        of Symbol
    | Pipe          of Symbol
    | LeftRedirect  of Symbol
    | RightRedirect of Symbol
    | Escape        of Symbol


type LookaheadToken =
    | Lookahead of Token * Token list
    | EOL


type JoinedToken =
    | Joined of Token
    | NotJoined of Token * Token


type SpecialTokenReadMode =
    | Match
    | Ignore

// TODO: figure out how to handle "ESCAPE".

type ParserState = {
    ReadMode: SpecialTokenReadMode
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


(*let addLocations (l0: Location) (l1: Location) =
    let (Location l0col, l0len) = l0
    let (Location _, l1len) = l1
    Location(l0col, (l0len + l1len)


let joinLiterals (tokA: Literal) (tokB: Literal) =
    let newLocation = addLocations tokA tokB
    let (Literal _, aText) = tokA
    let (Literal _, bText) = tokB
    Literal newLocation (aText + bText)


let tryJoinTokens tokenA tokenB =
    match (tokenA, tokenB) with
    | Literal(_, _), Literal(_, _) -> Joined ((joinLiterals tokenA tokenB))
    | CondAlways, CondAlways -> Joined CondSuccess
    | Pipe, Pipe             -> Joined CondOr
    | _, _                   -> NotJoined (tokenA, tokenB)





let lookahead (chars: Token list) =
    match chars with
    | [] -> EOL
    | head::tail ->
        Lookahead(head, tail)
*)

let pushToken (lst: Token list) (tok: Token) =
    tok :: lst


let rec tokenise (ctx: ParserState) =

    match ctx.Input with
    | head :: rest ->
        match head with
        | _ when ctx.ReadMode = EscapeOne ->
            tokenise {ctx with Tokens = (pushToken ctx.Tokens (Literal head)); ReadMode = }



let toSymbol ch loc =
    {
        Value = ch.ToString()
        Location = Location (Column loc, Length 1)
    }


let toSymbolList (str: string) =
    let chars = List.ofSeq str
    let locs  = [0..str.Length]
    (Seq.map2 toSymbol chars locs) |> Seq.toList

