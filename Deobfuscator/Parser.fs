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
    ParseTreeStack: ParseTree list
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


    let (|ParseTreeCommand|ParseTreePipe|ParseTreeAlways|Ignore|) (parseTree: ParseTree) =
        match parseTree with
        | Cmd cmd -> ParseTreeCommand cmd
        | Op operator ->
            match operator with
            | Pipe -> ParseTreePipe
            | CondAlways -> ParseTreeAlways
            | _ -> Ignore


    let pushAst x rest state =
        {state with Input = rest; ParseTreeStack = [x]}


    let pushParen p rest state =
        match state.ParseTreeStack with
        | [] ->
            pushAst p rest state

         | _ ->
            {state with Input = rest; ParseTreeStack = p :: state.ParseTreeStack}


    let pushLParen rest state =
        pushParen (Op OpenParen) rest state


    let pushRParen rest state =
        pushParen (Op CloseParen) rest state


    let pushPipe rest state =
        match state.ParseTreeStack with
        | [] ->
            pushAst (Op Pipe) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | ParseTreePipe ->
                {state with Input = rest; ParseTreeStack = (Op CondOr) :: restOfStack}

            | _ ->
                {state with Input = rest; ParseTreeStack = (Op Pipe) :: state.ParseTreeStack}


    let pushAmpersand rest state =
        match state.ParseTreeStack with
        | [] ->
            pushAst (Op CondAlways) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | ParseTreeAlways ->
                {state with Input = rest; ParseTreeStack = (Op CondSuccess) :: restOfStack}

            | _ ->
                {state with Input = rest; ParseTreeStack = (Op CondAlways) :: state.ParseTreeStack}


    let pushLRedirect rest state =
        match state.ParseTreeStack with
        | [] ->
            pushAst (Op LeftRedirect) rest state

        | _ ->
            {state with Input = rest; ParseTreeStack = (Op LeftRedirect) :: state.ParseTreeStack}


    let pushRRedirect rest state =
        match state.ParseTreeStack with
        | [] ->
            pushAst (Op RightRedirect) rest state

        | _ ->
            {state with Input = rest; ParseTreeStack = (Op RightRedirect) :: state.ParseTreeStack}


    let addCharToCmd (ch: char) (cmdlst: Token list) =
        let lit = Literal (ch.ToString())
        match cmdlst with
        | [] ->
            [lit]
        | head :: rest ->
            (head + lit) :: rest


    let pushCommand ch rest state =
        match state.ParseTreeStack with
        | [] ->
            pushAst (Cmd [Literal (ch.ToString())]) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | Op _ ->
                {state with Input = rest; CmdAppendMode = AppendToExisting; ParseTreeStack = (Cmd [Literal (ch.ToString())]) :: state.ParseTreeStack}

            | Cmd cmd when state.CmdAppendMode = AppendToExisting ->
                let updatedCmd = Cmd (addCharToCmd ch cmd)
                {state with Input = rest; ParseTreeStack = updatedCmd :: restOfStack}

            | Cmd cmd ->
                let newCmd = (Literal (ch.ToString())) :: cmd
                {state with Input = rest; CmdAppendMode = AppendToExisting; ParseTreeStack = (Cmd newCmd) :: restOfStack}


    let pushDelimiter ch rest state =
        let litcmd = Delimiter (ch.ToString())
        match state.ParseTreeStack with
        | [] ->
            pushAst (Cmd [litcmd]) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | Op _ ->
                {state with Input = rest; CmdAppendMode = StartNew; ParseTreeStack = (Cmd [litcmd]) :: state.ParseTreeStack }

            | Cmd cmd ->
                let newCmd = litcmd :: cmd
                {state with Input = rest; CmdAppendMode = StartNew; ParseTreeStack = (Cmd newCmd) :: restOfStack}


    let rec createParseTree (state: ParseState) =
        match state.Input with
        | [] ->
            List.map (fun mem ->
                match mem with
                | Cmd cmd -> Cmd (cmd |> List.rev)
                | _ -> mem) state.ParseTreeStack

        | head :: rest ->
            match head with
            | _ when state.Escape ->
                // The escape flag was set, so this char loses any special
                // meaning.
                createParseTree {(pushCommand head rest state) with Escape = false}

            | ESCAPE when state.Mode = MatchSpecial ->
                // Do not push '^', just set escape flag.
                createParseTree {state with Input = rest; Escape = true}

            | QUOTE when state.Mode = MatchSpecial ->
                // A quote toggles the matching of special chars.  The default state is to
                // MATCH special chars.  After the first QUOTE we IGNORE special chars.  This
                // mode flips each time a QUOTE is seen.
                createParseTree (pushCommand head rest {state with Input = rest; Escape = false; Mode = IgnoreSpecial})

            | QUOTE ->
                createParseTree (pushCommand head rest {state with Input = rest; Mode = MatchSpecial})

            | _ when state.Mode = IgnoreSpecial ->
                createParseTree (pushCommand head rest state)

            | DELIMITER ->
                createParseTree (pushDelimiter head rest state)

            | LPAREN ->
                createParseTree (pushLParen rest state)

            | RPAREN ->
                createParseTree (pushRParen rest state)

            | AMPERSAND ->
                createParseTree (pushAmpersand rest state)

            | PIPE ->
                createParseTree (pushPipe rest state)

            | LREDIRECT ->
                createParseTree (pushLRedirect rest state)

            | RREDIRECT ->
                createParseTree (pushRRedirect rest state)

            | _ ->
                createParseTree (pushCommand head rest state)



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


    let rec popGtEqOperators (oper: Operator) (opstack: Operator list) (accum: ParseTree list) =
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


    let rec infixToPrefix (ast: ParseTree list) (opstack: Operator list) (outstack: ParseTree list) =

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

        let swapParens parseTreeMember =
            match parseTreeMember with
            | Op OpenParen  -> Op CloseParen
            | Op CloseParen -> Op OpenParen
            | _ -> parseTreeMember

        let swappedParseTree = List.map swapParens ast

        match (infixToPrefix swappedParseTree [] []) with
        | Ok newParseTree -> Ok newParseTree
        | Error reason ->
            printfn "[ParseTree, toPrefix error!] --> %A" reason
            Error reason


    let parse (cmdstr: string) =
        let reader = {
            Mode = MatchSpecial
            Escape = false
            Input = (cmdstr |> List.ofSeq)
            ParseTreeStack = []
            CmdAppendMode = AppendToExisting
        }
        createParseTree reader |> toPrefix
