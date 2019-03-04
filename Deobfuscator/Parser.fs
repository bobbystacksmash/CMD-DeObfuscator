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


module AbstractSyntaxTree =

    type private Tree<'a> =
        | Br of 'a * 'a Tree * 'a Tree
        | Lf


    type private LastModifiedStack =
        | OperatorStack
        | CommandStack
        | Neither


    type Command = Command of Token list


    type private ASTState = {
        Tokens: Token list
        LastModifiedStack: LastModifiedStack
        Operators: Token list
        Commands: Command list
    }


    let private (|OPERATOR|COMMAND|DELIMITER|) token =
        match token with
        | CondAlways tok
        | CondSuccess tok
        | CondOr tok
        | Pipe tok
        | LeftRedirect tok
        | RightRedirect tok -> OPERATOR(token)
        | Delimiter delim -> DELIMITER(delim)
        | _ -> COMMAND(token)


    type ASTResult<'TSuccess, 'TFailure> =
        | Success of 'TSuccess
        | Failure of 'TFailure


    let private updateRemaining (ctx: ASTState) =
        {ctx with Tokens = ctx.Tokens.Tail}


    let private pushOperator ctx token =
        {ctx with Operators = (token :: ctx.Operators); LastModifiedStack = OperatorStack}


    let private addTokenToCommand (cmd: Command list) (token: Token) =
        let (Command headCmdBlock) = cmd.Head
        Command (List.append headCmdBlock [token]) :: cmd.Tail


    let private addTokenAsNewCommand cmd token =
        Command ([token]) :: cmd


    let private pushCommand ctx token =
        match ctx.LastModifiedStack with
        | Neither
        | OperatorStack ->
            {ctx with Commands = (addTokenAsNewCommand ctx.Commands token); LastModifiedStack = CommandStack}

        | CommandStack ->
            {ctx with Commands = (addTokenToCommand ctx.Commands token); LastModifiedStack = CommandStack}


    let rec private buildStacks state =
        match state.Tokens with
        | head :: rest ->
            match head with
            | DELIMITER delim ->
                buildStacks (state |> updateRemaining)

            | OPERATOR op ->
                buildStacks ((pushOperator state op) |> updateRemaining)

            | COMMAND  cmd ->
                buildStacks ((pushCommand  state cmd) |> updateRemaining)

        | [] ->
            printfn "--- COMMAND STACK ---"
            printfn "%A" state.Commands
            printfn "--- OPERATOR STACK ---"
            printfn "%A" state.Operators

            (state.Commands, state.Operators)


    let toAST tokens =

        let state = {
            Tokens = tokens
            Operators = []
            Commands = []
            LastModifiedStack = Neither
        }
        buildStacks state


