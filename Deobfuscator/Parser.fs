namespace Deobfuscator

type Operator =
    | Pipe
    | LeftRedirect
    | RightRedirect
    | CondOr
    | CondAlways
    | CondSuccess
    | LeftParen
    | RightParen

type Literal =
    | Literal of string
    static member (+) (a: Literal, b: Literal) =
        let (Literal strA) = a
        let (Literal strB) = b
        Literal(strA + strB)

type CommandAppendMode =
    | AppendToExisting
    | StartNew

type Command = {
    Commands: Literal list
    AppendMode: CommandAppendMode
}

type Ast =
    | Cmd of Command
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

    let (|CMDEMPTY|CMDLIT|CMDDELIM|) cmd =
        match cmd with
        | [] -> CMDEMPTY
        | head :: rest ->
            match head with
            | Lit l -> CMDLIT l
            | Delim d -> CMDDELIM d

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
        pushParen (Op LeftParen) rest state


    let pushRParen rest state =
        pushParen (Op RightParen) rest state


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


    let pushLiteral ch rest state =
        let newCmd = {Commands = [Literal (ch.ToString())]; AppendMode = AppendToExisting}
        match state.AstStack with
        | [] ->
            pushAst (Cmd newCmd) rest state

        | topOfStack :: restOfStack ->
            match topOfStack with
            | Op _ ->
                {state with Input = rest; AstStack = (Cmd newCmd) :: state.AstStack }

            | Cmd cmd ->
                match cmd.AppendMode with
                | AppendToExisting ->
                    (* TODO! *)


    let flipCommandAppendMode state =
        match state.AstStack with
        | [] -> state
        | head :: rest ->
            match head with
            | Op _ -> state
            | Cmd cmd ->
                match cmd.AppendMode with
                | AppendToExisting ->
                    let flipped = {cmd with AppendMode = StartNew}
                    {state with AstStack = (Cmd flipped) :: rest }

                | StartNew ->
                    let flipped = {cmd with AppendMode = AppendToExisting}
                    {state with AstStack = (Cmd flipped) :: rest}



    let rec makeAst (state: ParseState) =
        match state.Input with
        | [] -> state.AstStack
        | head :: rest ->
            match head with
            | _ when state.Escape ->
                // The escape flag was set, so this char loses any special
                // meaning.
                makeAst {(pushLiteral head rest state) with Escape = false}

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
                makeAst (pushLiteral head rest state)

            | DELIMITER ->
                makeAst {state with Input = rest; CmdReader = BuildNewCommand}

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
                makeAst (pushLiteral head rest state)



    (* AST Translation, from INFIX to PREFIX *)
    let private getPrecedence op =
        match op with
        | CondOr _
        | CondAlways _
        | CondSuccess _ -> 3
        | Pipe _
        | LeftRedirect _
        | RightRedirect _ -> 2
        | _ -> 1


    let (|HIGHER|LOWER|EQUAL|) (a, b) =
        let pA = getPrecedence a
        let pB = getPrecedence b
        if pA > pB then HIGHER
        elif pB < pB then LOWER
        else EQUAL


    let findClosingParen (stack: Operator list) =
        let rec find lst accum =
            match lst with
            | [] -> None
            | head :: rest ->
                match head with
                | LeftParen -> Some(accum |> List.rev |> List.map (fun x -> (Op x)), rest)
                | _ -> find rest (head :: accum)
        find stack []


    let rec infixToPrefix (ast: Ast list) (opstack: Operator list) (outstack: Ast list) =
        match ast with
        | [] ->
            let opers = List.map (fun op -> Op(op)) opstack
            Ok (opers @ outstack)

        | head :: rest ->
            match head with
            | Cmd cmd ->
                infixToPrefix rest opstack (head :: outstack)

            | Op LeftParen ->
                infixToPrefix rest (LeftParen :: opstack) outstack

            | Op RightParen ->
                match findClosingParen opstack with
                | Some (ops, remaining) ->
                    infixToPrefix rest remaining (ops @ outstack)
                | None ->
                    Error UnbalancedParenthesis

            | Op oper when opstack.Length = 0 ->
                infixToPrefix rest (oper :: opstack) outstack

            | Op oper ->
                match (oper, opstack.Head) with
                | LOWER
                | EQUAL ->
                    infixToPrefix rest opstack.Tail (head :: outstack)

                | HIGHER ->
                    infixToPrefix rest (oper :: opstack) outstack


    let convertAstToPrefix ast =
        match (infixToPrefix ast [] []) with
        | Ok newAst -> newAst
        | Error reason ->
            printfn "Error!"
            ast


    let tokenise (cmdstr: string) =
        let reader = {
            Mode = MatchSpecial
            Escape = false
            Input = (cmdstr |> List.ofSeq)
            AstStack = []
            CmdReader = BuildNewCommand
        }
        makeAst reader