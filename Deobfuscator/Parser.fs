namespace Deobfuscator

type Operator =
    | Pipe
    | LeftRedirect
    | RightRedirect
    | CondOr
    | CondAlways
    | CondSuccess
    | OpenParen
    | CloseParen

type Literal =
    | Literal of string
    static member (+) (a: Literal, b: Literal) =
        let (Literal strA) = a
        let (Literal strB) = b
        Literal(strA + strB)

type CommandAppendMode =
    | AppendToExisting
    | StartNew

type Ast =
    | Cmd of Literal list
    | Op of Operator

type CommandExpr =
    | Command of Literal list
    | Oper of Operator

type ReadMode =
    | MatchSpecial
    | IgnoreSpecial

type ParseState = {
    Mode: ReadMode
    Escape: bool
    Input: char list
    AstStack: Ast list
    CmdAppendMode: CommandAppendMode
}

type ParseStatus =
    | UnbalancedParenthesis

module Tokeniser =

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


    let (|ASTCOMMAND|ASTPIPE|ASTALWAYS|IGNORE|) (ast: Ast) =
        match ast with
        | Cmd cmd -> ASTCOMMAND cmd
        | Op operator ->
            match operator with
            | Pipe -> ASTPIPE
            | CondAlways -> ASTALWAYS
            | _ -> IGNORE


    let pushAst x rest state =
        {state with Input = rest; AstStack = [x]}


    let pushParen p rest state =
        match state.AstStack with
        | [] ->
            pushAst p rest state

         | _ ->
            {state with Input = rest; AstStack = p :: state.AstStack}


    let pushLParen rest state =
        pushParen (Op OpenParen) rest state


    let pushRParen rest state =
        pushParen (Op CloseParen) rest state


    let pushPipe rest state =
        match state.AstStack with
        | [] ->
            pushAst (Op Pipe) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | ASTPIPE ->
                {state with Input = rest; AstStack = (Op CondOr) :: restOfStack}

            | _ ->
                {state with Input = rest; AstStack = (Op Pipe) :: state.AstStack}


    let pushAmpersand rest state =
        match state.AstStack with
        | [] ->
            pushAst (Op CondAlways) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | ASTALWAYS ->
                {state with Input = rest; AstStack = (Op CondSuccess) :: restOfStack}

            | _ ->
                {state with Input = rest; AstStack = (Op CondAlways) :: state.AstStack}


    let pushLRedirect rest state =
        match state.AstStack with
        | [] ->
            pushAst (Op LeftRedirect) rest state

        | _ ->
            {state with Input = rest; AstStack = (Op LeftRedirect) :: state.AstStack}


    let pushRRedirect rest state =
        match state.AstStack with
        | [] ->
            pushAst (Op RightRedirect) rest state

        | _ ->
            {state with Input = rest; AstStack = (Op RightRedirect) :: state.AstStack}


    let addCharToCmd (ch: char) (cmdlst: Literal list) =
        let lit = Literal (ch.ToString())
        match cmdlst with
        | [] ->
            [lit]
        | head :: rest ->
            (head + lit) :: rest


    let pushCommand ch rest state =
        match state.AstStack with
        | [] ->
            pushAst (Cmd [Literal (ch.ToString())]) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | Op _ ->
                {state with Input = rest; CmdAppendMode = AppendToExisting; AstStack = (Cmd [Literal (ch.ToString())]) :: state.AstStack}

            | Cmd cmd when state.CmdAppendMode = AppendToExisting ->
                let updatedCmd = Cmd (addCharToCmd ch cmd)
                {state with Input = rest; AstStack = updatedCmd :: restOfStack}

            | Cmd cmd ->
                let newCmd = (Literal (ch.ToString())) :: cmd
                {state with Input = rest; CmdAppendMode = AppendToExisting; AstStack = (Cmd newCmd) :: restOfStack}


    let rec makeAst (state: ParseState) =
        //printfn "AST > %A" state.AstStack
        match state.Input with
        | [] ->
            List.map (fun mem ->
                match mem with
                | Cmd cmd -> Cmd (cmd |> List.rev)
                | _ -> mem) state.AstStack

        | head :: rest ->
            match head with
            | _ when state.Escape ->
                // The escape flag was set, so this char loses any special
                // meaning.
                makeAst {(pushCommand head rest state) with Escape = false}

            | ESCAPE when state.Mode = MatchSpecial ->
                // Do not push '^', just set escape flag.
                makeAst {state with Input = rest; Escape = true}

            | QUOTE when state.Mode = MatchSpecial ->
                // A quote toggles the matching of special chars.  The default state is to
                // MATCH special chars.  After the first QUOTE we IGNORE special chars.  This
                // mode flips each time a QUOTE is seen.
                makeAst {state with Input = rest; Escape = false; Mode = IgnoreSpecial}

            | QUOTE ->
                makeAst {state with Input = rest; Mode = MatchSpecial}

            | _ when state.Mode = IgnoreSpecial ->
                makeAst (pushCommand head rest state)

            | DELIMITER ->
                makeAst {state with Input = rest; CmdAppendMode = StartNew}

            | LPAREN ->
                makeAst (pushLParen rest state)

            | RPAREN ->
                makeAst (pushRParen rest state)

            | AMPERSAND ->
                makeAst (pushAmpersand rest state)

            | PIPE ->
                makeAst (pushPipe rest state)

            | LREDIRECT ->
                makeAst (pushLRedirect rest state)

            | RREDIRECT ->
                makeAst (pushRRedirect rest state)

            | _ ->
                makeAst (pushCommand head rest state)



    (* AST Translation, from INFIX to PREFIX *)
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


    let rec popGtEqOperators (oper: Operator) (opstack: Operator list) (accum: Ast list) =
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


    let rec infixToPrefix (ast: Ast list) (opstack: Operator list) (outstack: Ast list) =

        //printfn "---- infixToPrefix ----"
        //printfn "AST>%A" ast
        //printfn "XXXX OPS>%A" opstack
        //printfn "OUT>%A" outstack
        //printfn "########################"

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


    let convertAstToPrefix ast =

        let swapParens astMember =
            match astMember with
            | Op OpenParen  -> Op CloseParen
            | Op CloseParen -> Op OpenParen
            | _ -> astMember

        let swappedAst = List.map swapParens ast

        match (infixToPrefix swappedAst [] []) with
        | Ok newAst -> newAst
        | Error reason ->
            printfn "Error! --> %A" reason
            ast


    let tokenise (cmdstr: string) =
        let reader = {
            Mode = MatchSpecial
            Escape = false
            Input = (cmdstr |> List.ofSeq)
            AstStack = []
            CmdAppendMode = AppendToExisting
        }
        makeAst reader |> convertAstToPrefix