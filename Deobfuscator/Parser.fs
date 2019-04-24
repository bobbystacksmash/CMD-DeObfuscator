namespace Deobfuscator

open Deobfuscator.DomainTypes
open System.Text.RegularExpressions

type CommandAppendMode =
    | AppendToExisting
    | StartNew

type CommandExpr =
    | Command of Token list
    | Oper of Operator

type ReadMode =
    | MatchSpecial
    | IgnoreSpecial

type ParseState = {
    Mode: ReadMode
    Escape: bool
    Input: char list
    ParseResultStack: ParseResult list
    CmdAppendMode: CommandAppendMode
}

type ParseStatus =
    | UnbalancedParenthesis
    | ErrorEmptyCommandBlock


module Parser =

    let (|PIPE|_|) ch =
        if ch = '|' then Some PIPE
        else None

    let (|LREDIRECT|_|) ch =
        if ch = '<' then Some LREDIRECT
        else None

    let (|RREDIRECT|_|) ch =
        if ch = '>' then Some RREDIRECT
        else None

    let (|AMPERSAND|_|) ch =
        if ch = '&' then Some AMPERSAND
        else None

    let (|LPAREN|_|) ch =
        if ch = '(' then Some LPAREN
        else None

    let (|RPAREN|_|) ch =
        if ch = ')' then Some RPAREN
        else None

    let (|ESCAPE|_|) ch =
        if ch = '^' then Some ESCAPE
        else None

    let (|QUOTE|_|) ch =
        if ch = '"' then Some QUOTE
        else None

    let (|DELIMITER|_|) sym =
        match sym with
        | ','
        | '='
        | ' '
        | ';' -> Some(DELIMITER)
        | _   -> None


    let (|ParseResultCommand|ParseResultPipe|ParseResultAlways|Ignore|) (parseResult: ParseResult) =
        match parseResult with
        | Cmd cmd -> ParseResultCommand cmd
        | Op operator ->
            match operator with
            | Pipe -> ParseResultPipe
            | CondAlways -> ParseResultAlways
            | _ -> Ignore


    let pushAst x rest state =
        {state with Input = rest; ParseResultStack = [x]}


    let pushParen p rest state =
        match state.ParseResultStack with
        | [] ->
            pushAst p rest state

         | _ ->
            {state with Input = rest; ParseResultStack = p :: state.ParseResultStack}


    let pushLParen rest state =
        pushParen (Op OpenParen) rest state


    let pushRParen rest state =
        pushParen (Op CloseParen) rest state


    let pushPipe rest state =
        match state.ParseResultStack with
        | [] ->
            pushAst (Op Pipe) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | ParseResultPipe ->
                {state with Input = rest; ParseResultStack = (Op CondOr) :: restOfStack}

            | _ ->
                {state with Input = rest; ParseResultStack = (Op Pipe) :: state.ParseResultStack}


    let pushAmpersand rest state =
        match state.ParseResultStack with
        | [] ->
            pushAst (Op CondAlways) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | ParseResultAlways ->
                {state with Input = rest; ParseResultStack = (Op CondSuccess) :: restOfStack}

            | _ ->
                {state with Input = rest; ParseResultStack = (Op CondAlways) :: state.ParseResultStack}


    let pushLRedirect rest state =
        match state.ParseResultStack with
        | [] ->
            pushAst (Op LeftRedirect) rest state

        | _ ->
            {state with Input = rest; ParseResultStack = (Op LeftRedirect) :: state.ParseResultStack}


    let pushRRedirect rest state =
        match state.ParseResultStack with
        | [] ->
            pushAst (Op RightRedirect) rest state

        | _ ->
            {state with Input = rest; ParseResultStack = (Op RightRedirect) :: state.ParseResultStack}


    let addCharToCmd (ch: char) (cmdlst: Token list) =
        let lit = Literal (ch.ToString())
        match cmdlst with
        | [] ->
            [lit]
        | head :: rest ->
            (head + lit) :: rest


    let pushCommand ch rest state =
        match state.ParseResultStack with
        | [] ->
            pushAst (Cmd [Literal (ch.ToString())]) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | Op _ ->
                {state with Input = rest; CmdAppendMode = AppendToExisting; ParseResultStack = (Cmd [Literal (ch.ToString())]) :: state.ParseResultStack}

            | Cmd cmd when state.CmdAppendMode = AppendToExisting ->
                let updatedCmd = Cmd (addCharToCmd ch cmd)
                {state with Input = rest; ParseResultStack = updatedCmd :: restOfStack}

            | Cmd cmd ->
                let newCmd = (Literal (ch.ToString())) :: cmd
                {state with Input = rest; CmdAppendMode = AppendToExisting; ParseResultStack = (Cmd newCmd) :: restOfStack}


    let pushDelimiter ch rest state =
        let litcmd = Delimiter (ch.ToString())
        match state.ParseResultStack with
        | [] ->
            pushAst (Cmd [litcmd]) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | Op _ ->
                {state with Input = rest; CmdAppendMode = StartNew; ParseResultStack = (Cmd [litcmd]) :: state.ParseResultStack }

            | Cmd cmd ->
                let newCmd = litcmd :: cmd
                {state with Input = rest; CmdAppendMode = StartNew; ParseResultStack = (Cmd newCmd) :: restOfStack}


    let rec createParseResult (state: ParseState) =
        match state.Input with
        | [] ->
            List.map (fun mem ->
                match mem with
                | Cmd cmd -> Cmd (cmd |> List.rev)
                | _ -> mem) state.ParseResultStack

        | head :: rest ->
            match head with
            | _ when state.Escape ->
                // The escape flag was set, so this char loses any special
                // meaning.
                createParseResult {(pushCommand head rest state) with Escape = false}

            | ESCAPE when state.Mode = MatchSpecial ->
                // Do not push '^', just set escape flag.
                createParseResult {state with Input = rest; Escape = true}

            | QUOTE when state.Mode = MatchSpecial ->
                // A quote toggles the matching of special chars.  The default state is to
                // MATCH special chars.  After the first QUOTE we IGNORE special chars.  This
                // mode flips each time a QUOTE is seen.
                createParseResult (pushCommand head rest {state with Input = rest; Escape = false; Mode = IgnoreSpecial})

            | QUOTE ->
                createParseResult (pushCommand head rest {state with Input = rest; Mode = MatchSpecial})

            | _ when state.Mode = IgnoreSpecial ->
                createParseResult (pushCommand head rest state)

            | DELIMITER ->
                createParseResult (pushDelimiter head rest state)

            | LPAREN ->
                createParseResult (pushLParen rest state)

            | RPAREN ->
                createParseResult (pushRParen rest state)

            | AMPERSAND ->
                createParseResult (pushAmpersand rest state)

            | PIPE ->
                createParseResult (pushPipe rest state)

            | LREDIRECT ->
                createParseResult (pushLRedirect rest state)

            | RREDIRECT ->
                createParseResult (pushRRedirect rest state)

            | _ ->
                createParseResult (pushCommand head rest state)



    (* Parse Tree Translation, from INFIX to PREFIX *)
    let private getPrecedence op =
        match op with
        | OpenParen _ -> 0
        | CloseParen _ -> 0
        | CondOr _
        | CondAlways _
        | CondSuccess _ -> 3
        | Pipe _
        | LeftRedirect _
        | RightRedirect _ -> 2


    let (|HIGHER|LOWER|EQUAL|) (a, b) =
        let pA = getPrecedence a
        let pB = getPrecedence b
        if pA > pB then HIGHER
        elif pB < pB then LOWER
        else EQUAL


    let findOpsUntilOpeningParen (stack: Operator list) =
        let rec find lst accum =
            match lst with
            | [] -> None
            | head :: rest ->
                match head with
                | OpenParen -> Some(accum |> List.rev |> List.map (fun x -> (Op x)), rest)
                | _ -> find rest (head :: accum)

        match find stack [] with
        | Some operators -> Ok operators
        | None -> Error UnbalancedParenthesis


    let rec popGtEqOperators (oper: Operator) (opstack: Operator list) (accum: ParseResult list) =
        match opstack with
        | [] -> (accum, [])
        | head :: rest ->
            match head with
            | OpenParen ->
                (accum, opstack)

            | _ ->
                match (oper, opstack.Head) with
                | HIGHER
                | EQUAL ->
                    // These go in to the accumulator, destined for the outstack.
                    popGtEqOperators oper opstack.Tail (Op opstack.Head :: accum)
                | LOWER ->
                    (accum, opstack)


    let rec infixToPrefix (ast: ParseResult list) (opstack: Operator list) (outstack: ParseResult list) =

        match ast with
        | [] ->
            let opers = List.map (fun op -> Op(op)) opstack
            Ok (opers @ outstack)

        | head :: rest ->
            match head with
            | Cmd cmd ->
                infixToPrefix rest opstack (head :: outstack)

            | Op OpenParen ->
                infixToPrefix rest (OpenParen :: opstack) outstack

            | Op CloseParen ->
                match findOpsUntilOpeningParen opstack with
                | Ok (outstackOpers, remainingOpers) ->
                    infixToPrefix rest remainingOpers (outstackOpers @ outstack)
                | Error reason ->
                    Error reason

            | Op oper when opstack.Length = 0 ->
                infixToPrefix rest (oper :: opstack) outstack

            | Op oper ->
                let (higherPrecedenceOpers, remainingOpstack) = popGtEqOperators oper opstack []
                infixToPrefix rest (oper :: remainingOpstack) (higherPrecedenceOpers @ outstack)


    let private toPrefix ast =

        let swapParens parseResultMember =
            match parseResultMember with
            | Op OpenParen  -> Op CloseParen
            | Op CloseParen -> Op OpenParen
            | _ -> parseResultMember

        let swappedParseResult = List.map swapParens ast

        match (infixToPrefix swappedParseResult [] []) with
        | Ok newParseResult -> Ok newParseResult
        | Error reason ->
            printfn "[ParseResult, toPrefix error!] --> %A" reason
            Error reason


    let parse (cmdstr: string) =
        let reader = {
            Mode = MatchSpecial
            Escape = false
            Input = (cmdstr |> List.ofSeq)
            ParseResultStack = []
            CmdAppendMode = AppendToExisting
        }
        createParseResult reader |> toPrefix
