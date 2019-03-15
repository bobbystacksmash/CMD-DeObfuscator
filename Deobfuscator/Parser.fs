namespace Deobfuscator

type private Column = Column of int
type private Length = Length of int
type private Location = Location of Column * Length

type Symbol = Symbol of string

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


type private Concat =
    | ConcatSuccess of Token
    | ConcatFailure


type private LookaheadToken =
    | Lookahead of Token * Token list
    | NoMoreTokens


type private TokenReadMode =
    | MatchSpecial
    | IgnoreSpecial


type private ParserState = {
    ReadMode: TokenReadMode
    Escape: bool
    Tokens: Token list
    Input: Symbol list
}


module Tokeniser =

    let (|LPAREN|_|) sym =
        if sym = Symbol("(") then Some(LeftParen sym) else None

    let (|RPAREN|_|) sym =
        if sym = Symbol(")") then Some(RightParen sym) else None

    let (|PIPE|_|) sym =
        if sym = Symbol("|") then Some(Pipe sym) else None

    let (|CONDALWAYS|_|) sym =
        if sym = Symbol("&") then Some(CondAlways sym) else None

    let (|LREDIRECT|_|) sym =
        if sym = Symbol("<") then Some(LeftRedirect sym) else None

    let (|RREDIRECT|_|) sym =
        if sym = Symbol(">") then Some(RightRedirect sym) else None

    let (|ESCAPE|_|) sym =
        if sym = Symbol("^") then Some(Escape sym) else None

    let (|QUOTE|_|) sym =
        if sym = Symbol("\"") then Some(Quote sym) else None

    let (|AMPERSAND|_|) sym =
        if sym = Symbol("&") then Some(CondAlways sym) else None

    let (|DELIMITER|_|) sym =
        match sym with
        | Symbol(",")
        | Symbol("=")
        | Symbol(" ")
        | Symbol(";") -> Some(Delimiter sym)
        | _   -> None

    let private pushToken pstate tok rest =
        {
            pstate with
                Tokens = (tok :: pstate.Tokens);
                Input  = rest
        }

    let private readModeMatch pstate =
        {pstate with ReadMode = MatchSpecial}


    let private readModeIgnore pstate =
        {pstate with ReadMode = IgnoreSpecial}


    let private escapeOff pState =
        {pState with Escape = false}


    let private escapeOn pState =
        {pState with Escape = true}


    let private concatSymbols symA symB =
        let (Symbol a) = symA
        let (Symbol b) = symB
        Symbol(a + b)


    let private concatTokens tokA tokB =
        if tokA.GetType() <> tokB.GetType() then
            ConcatFailure
        else
            match (tokA, tokB) with
            | Literal l0, Literal l1 ->
                ConcatSuccess (Literal(concatSymbols l0 l1))

            | Delimiter d0, Delimiter d1 ->
                ConcatSuccess (Delimiter(concatSymbols d0 d1))

            | CondAlways c0, CondAlways c1 ->
                ConcatSuccess (CondSuccess(concatSymbols c0 c1))

            | Pipe p0, Pipe p1 ->
                ConcatSuccess (CondOr(concatSymbols p0 p1))

            | _ ->
                ConcatFailure


    let private lookahead (tokens: Token list) =
        if tokens.Length = 0 then NoMoreTokens
        else Lookahead(tokens.Head, tokens.Tail)


    let private catTokens (tokens: Token list) =
        let rec doJoin (todo: Token list) (accum: Token list) =
            if todo.Length > 0 && accum.Length > 0 then
                match concatTokens accum.Head todo.Head with
                | ConcatSuccess newTok -> doJoin todo.Tail (newTok :: accum.Tail)
                | ConcatFailure -> doJoin todo.Tail (todo.Head :: accum)
            elif accum.Length = 0 then
                doJoin todo.Tail (todo.Head :: accum)
            else
                accum |> List.rev
        doJoin tokens []


    let tokenise (cmdstr: string) =

        let rec symbolsToTokens (pState: ParserState) =

            match pState.Input with
            | head :: rest ->
                match head with
                | _ when pState.Escape ->
                    // The 'escape' flag is set -- ignore any special meaning associated with
                    // the current char, and save as a literal, resetting the escape flag.
                    symbolsToTokens ((pushToken pState (Literal head) rest) |> escapeOff)

                | ESCAPE esc when pState.ReadMode = MatchSpecial ->
                    // We don't push on the '^' (escape) symbol.
                    symbolsToTokens {pState with Input = rest; Escape = true}

                | QUOTE qt when pState.ReadMode = MatchSpecial ->
                    // A quote toggles the matching of special chars.  The default state is to
                    // MATCH special chars.  After the first QUOTE we IGNORE special chars.  This
                    // mode flips each time a QUOTE is seen.
                    symbolsToTokens ((pushToken pState qt rest) |> escapeOff |> readModeIgnore)

                | QUOTE qt ->
                    symbolsToTokens ((pushToken pState qt rest) |> readModeMatch)

                | _ when pState.ReadMode = IgnoreSpecial ->
                    // We're ignoring special chars
                    symbolsToTokens (pushToken pState (Literal head) rest)

                | LPAREN    sym
                | RPAREN    sym
                | LREDIRECT sym
                | RREDIRECT sym
                | DELIMITER sym
                | AMPERSAND sym
                | PIPE      sym ->
                    symbolsToTokens (pushToken pState sym rest)

                | _ ->
                    symbolsToTokens (pushToken pState (Literal head) rest)

            | _ -> pState.Tokens |> List.rev |> catTokens

        let symbols = cmdstr |> List.ofSeq  |> List.map (fun chr -> Symbol(chr.ToString()))
        let pstate = {
            ReadMode = MatchSpecial
            Escape   = false
            Tokens   = []
            Input    = symbols
        }
        symbolsToTokens pstate


module Translator =

    let reverseTokens tokens =
        tokens |> List.rev |> List.map (fun token ->
            match token with
            | LeftParen  _ -> RightParen (Symbol ")")
            | RightParen _ -> LeftParen  (Symbol "(")
            | _ -> token)


    let private getPrecedence token =
        match token with
        | CondOr _
        | CondAlways _
        | CondSuccess _ -> 3
        | Pipe _
        | LeftRedirect _
        | RightRedirect _ -> 2
        | _ -> 0


    let (|HIGHER|LOWER|EQUAL|) (tokA, tokB) =
        let pA = getPrecedence tokA
        let pB = getPrecedence tokB
        if pA > pB then HIGHER
        elif pB < pB then LOWER
        else EQUAL


    let (|LPAREN|RPAREN|OPERATOR|OPERAND|) token =
        match token with
        | LeftParen _ -> LPAREN
        | RightParen _ -> RPAREN
        | Pipe _
        | CondOr _
        | CondAlways _
        | CondSuccess _ -> OPERATOR
        | _ -> OPERAND


    let findClosingParen stack =
        let rec find lst accum =
            match lst with
            | [] -> None
            | head :: rest ->
                match head with
                | LeftParen _ -> Some (accum |> List.rev, rest)
                | _ -> find rest (head :: accum)
        find stack []

    type ConversionStatus =
        | UnbalancedParenthesis

    let rec private infixToPostFix tokens (opstack: Token list) (outstack: Token list) =
        match tokens with
        | [] -> Ok (opstack @ outstack)
        | head :: rest ->
            match head with
            | OPERAND  ->
                infixToPostFix rest opstack (head :: outstack)

            | LPAREN ->
                infixToPostFix rest (head :: opstack) outstack

            | RPAREN ->
                match findClosingParen opstack with
                | Some (opers, remainingOpers) ->
                    infixToPostFix rest remainingOpers (opers @ outstack)
                | None ->
                    Error UnbalancedParenthesis

            | OPERATOR when opstack.Length = 0 ->
                infixToPostFix rest (head :: opstack) outstack

            | OPERATOR ->
                match (head, opstack.Head) with
                | LOWER
                | EQUAL ->
                    infixToPostFix rest opstack.Tail (opstack.Head :: outstack)

                | HIGHER ->
                    infixToPostFix rest (head :: opstack) outstack


    type CommandExpression =
        | Operator of Token // TODO: type system can help here!
        | Expr of Token list
        | LeftParen of Token // TODO: type system can help here!
        | RightParen of Token // TODO: type system can help here!


    let rec private group tokens (accum: CommandExpression list) =
        match tokens with
        | [] -> accum
        | head :: rest ->
            match head with
            | OPERATOR ->
                group rest ((Operator head) :: accum)

            | LPAREN ->
                group rest ((LeftParen head) :: accum)

            | RPAREN ->
                group rest ((RightParen head) :: accum)

            | _ ->
                match accum with
                | [] -> group rest ((Expr [head]) :: accum)
                | tos :: restofaccum ->
                    match tos with
                    | Expr currExp ->
                        group rest ((Expr (head :: currExp)) :: restofaccum)
                    | _ ->
                        group rest ((Expr [head]) :: accum)


    let translate tokens =

        let isNotDelim tok =
            match tok with
            | Delimiter _ -> false
            | _ -> true

        let filtered = List.filter isNotDelim tokens

        printfn "---- Grouping ----"
        printfn "%A" (group filtered)

        (*match (infixToPostFix (reverseTokens filtered) [] []) with
        | Ok outstack ->
            printfn "Success! ->>>> %A" outstack

        | Error _ ->
            printfn "Failed."*)
