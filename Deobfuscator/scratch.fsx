open System.Reflection.Emit
type Token =
    | LeftParen     of string // (
    | RightParen    of string // )
    | Literal       of string
    | Delimiter     of string
    | CondSuccess   of string // &&
    | CondAlways    of string // &
    | CondOr        of string // ||
    | LeftRedirect  of string // <
    | RightRedirect of string // >
    | Pipe          of string // |
    | Quote         of string // "
    static member (+) (a, b) =
        match a, b with
        | Literal a0, Literal a1 -> Literal (a0 + a1)
        | Delimiter d0, Delimiter d1 -> Delimiter (d0 + d1)
        | CondAlways c0, CondAlways c1 -> CondSuccess (c0 + c1)
        | Pipe p0, Pipe p1 -> CondOr (p0 + p1)
        | _ -> failwith "invalid arguments to perform token join"

    static member CanConcat =
        function
            | Literal _
            | Delimiter _
            | Pipe _
            | CondAlways _ -> true
            | _ -> false

type NodeType =
    | Oper of Token
    | Command  of Token list

type Matcher =
    | MatchingSpecialChars
    | IgnoringSpecialChars

type Stack =
    | Operand
    | Operator
    | Neither

type Tree =
    | Empty
    | Node of value: NodeType * left: Tree * right: Tree

type TokeniserState = {
    Escape: bool
    Mode: Matcher
    LastModifiedStack: Stack
    OperandStack: Token list list
    OperatorStack: Token list
}


let sameType a b = a.GetType() = b.GetType()

let (|ESC|QUOTE|LPAREN|RPAREN|LREDIRECT|RREDIRECT|OTHER|) str =
    match str with
    | "^"  -> ESC   str
    | "\"" -> QUOTE str
    | "("  -> LPAREN str
    | ")"  -> RPAREN str
    | "<"  -> LREDIRECT str
    | ">"  -> RREDIRECT str
    | _    -> OTHER str

let (|AMPERSAND|PIPE|OTHER|) str =
    match str with
    | "&" -> AMPERSAND str
    | "|" -> PIPE str
    | _   -> OTHER str

let (|DELIM|OTHER|) chr =
    match chr with
    | ","
    | "="
    | " "
    | ";" -> DELIM chr
    | _   -> OTHER chr


let popStack (stack: Token list) (num: int) =

    let rec pop (s: Token list) n acc =
        if s.Length = 0 then acc
        else
            printfn "Popping -> %A" s.Tail
            pop (s.Tail) (n - 1) (s.Head :: acc)

    (pop stack num)


let rec popN (stack: 'a list) n =
    match stack with
    | [] -> stack
    | _ when n = 0 -> stack
    | _ -> popN (stack.Tail) (n-1)

let catStack (stack: Token list) =
    if stack.Length > 1 then
        let s0 = stack.[0]
        let s1 = stack.[1]
        if Token.CanConcat(s0) && sameType s0 s1 then
            (s1 + s0) :: (popN stack 2)
        else
            stack
    else
        stack


let pushOperator (ctx: TokeniserState) (token: Token) =
    (token :: ctx.OperatorStack) |> catStack


let pushOperand (ctx: TokeniserState) (token: Token) =

    printfn "-----------------------------"
    printfn "Operator Stack = %A" ctx.OperatorStack
    printfn "Operand Stack  = %A" ctx.OperandStack
    printfn "Last stack = %A" ctx.LastModifiedStack
    printfn "Pushing Operand -> %A" token

    let operandsGroup = ctx.OperandStack.Head

    if ctx.OperandStack.Length = 0 then
        ((token :: operandsGroup) |> catStack) :: ctx.OperandStack.Tail
    elif ctx.LastModifiedStack = Operand or ctx.LastModifiedStack = Neither then
        // When the last modified stack is the operand stack, we shove this token
        // in to the current group.
        ((token :: operandsGroup) |> catStack) :: ctx.OperandStack.Tail
    else
        printfn "!! OPERATOR STACK WAS LAST MODIFIED !!"
        // The last thing we saw was an Operator, so start a new
        // operands group...
        [token] :: ctx.OperandStack


let rec tokeniseCmd (cmdstr: string list) (ctx: TokeniserState) =

    let catOperators (stack: Token list) =
        match ctx.LastModifiedStack with
        | Operator when ctx.LastModifiedStack = Operator -> catStack stack
        | _        -> stack

    match cmdstr with
    | chr::rest ->
        match chr with
        | _ when ctx.Escape ->
            let newStack = (pushOperand ctx (Literal chr))
            tokeniseCmd rest {ctx with Escape=false; LastModifiedStack = Operand; OperandStack = newStack}

        | ESC _ when ctx.Mode = MatchingSpecialChars ->
            tokeniseCmd rest {ctx with Escape=true}

        | QUOTE _ when ctx.Mode = MatchingSpecialChars ->
            let newStack = pushOperand ctx (Quote "\"")
            tokeniseCmd rest {ctx with Escape=false; LastModifiedStack=Operand; Mode=IgnoringSpecialChars; OperandStack=newStack}

        | QUOTE _ ->
            let newStack = pushOperand ctx (Quote "\"")
            tokeniseCmd rest {ctx with Mode=MatchingSpecialChars; LastModifiedStack=Operand; OperandStack=newStack}

        | _ when ctx.Mode = IgnoringSpecialChars ->
            let newStack = pushOperand ctx (Literal chr)
            tokeniseCmd rest {ctx with OperandStack = newStack; LastModifiedStack=Operand}

        | LPAREN lp ->
            let newStack = pushOperator ctx (LeftParen lp)
            tokeniseCmd rest {ctx with OperatorStack = newStack; LastModifiedStack=Operator}

        | RPAREN rp ->
            let newStack = pushOperator ctx (RightParen rp)
            tokeniseCmd rest {ctx with OperatorStack = newStack; LastModifiedStack=Operator}

        | LREDIRECT lrd ->
            let newStack = pushOperator ctx (LeftRedirect lrd)
            tokeniseCmd rest {ctx with OperatorStack = newStack; LastModifiedStack=Operator}

        | RREDIRECT rrd ->
            let newStack = pushOperator ctx (RightRedirect rrd)
            tokeniseCmd rest {ctx with OperatorStack = newStack; LastModifiedStack=Operator}

        | DELIM delim ->
            let newStack = pushOperand ctx (Delimiter delim)
            tokeniseCmd rest {ctx with OperandStack = newStack; LastModifiedStack=Operand}

        | AMPERSAND amp ->
            let newStack = pushOperator ctx (CondAlways amp)
            tokeniseCmd rest {ctx with OperatorStack = newStack; LastModifiedStack=Operator}

        | PIPE pipe ->
            let newStack = pushOperator ctx (Pipe pipe)
            tokeniseCmd rest {ctx with OperatorStack = newStack; LastModifiedStack=Operator}

        | _ ->
            let newStack = (pushOperand ctx (Literal chr))
            tokeniseCmd rest {ctx with OperandStack = newStack; LastModifiedStack=Operand}
    | _ -> ctx


let makeAST (operatorStack: Token list) (operandGroups: Token list list) =

    printfn "MK AST STACKS (OPERATOR) %A" operatorStack
    printfn "MK AST STACKS (OPERAND)  %A" operandGroups

    let xx =
        operandGroups
        |> List.map(fun group ->
            List.map(fun operand -> (Node ((Command operand), Empty, Empty))
            )
        )


    (*let operandStack = operandGroups |> List.map (fun op -> (Node ((Command [op]), Empty, Empty)))

    // Take one from operator stack, push on to operand (tree) stack.
    let addNode (stack: Tree list) newThing =
        let rhs = stack.[0]
        let lhs = stack.[1]
        let node = Node ((Oper newThing), lhs, rhs)
        node :: (popN stack 2)

    let rec mkAST operators (operands: Tree list) =
        match operators with
        | [] -> operands.Head
        | _ ->
            let x = (addNode operands operators.Head)
            mkAST (operators.Tail) x

    mkAST operatorStack operandStack*)




let tokenise cmdstr =
    let state = {
        Escape = false
        Mode = MatchingSpecialChars
        LastModifiedStack = Neither
        OperandStack = [[]]; OperatorStack = []
    }
    let ast = Empty
    let ctx =
        cmdstr.ToString()
            |> Seq.toList
            |> List.map (fun ch -> ch.ToString())
            |> (fun x -> tokeniseCmd x state)
            |> (fun ctx -> makeAST ctx.OperatorStack ctx.OperandStack)

    printfn "#########################################"
    printfn "#########################################"
    printfn "CTX -> %A" ctx
    printfn "#########################################"
    printfn "#########################################"

tokenise "foo && bar baz"
