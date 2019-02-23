//               T O K E N I S E R
//              ~-~-~-~-~-~-~-~-~-~
//
// HOW IT WORKS
// ============
//
// The entry point to the tokeniser is the function `tokenise', which accepts
// a single command string, though the real work (internal API) is all handled
// by `tokeniseCmd'.
//
// Tokenising happens in a few different phases.  From a high-level, these are:
//
//   1. Split input string in to individual characters.
//   2. For each char, assign it a Token type (LeftParen, Literal, Quote, ...)
//   3. IF token is an operator, push on to the OPERATOR stack.
//   4. IF token is an operand, push on to the OPERAND stack.
//   5. When all tokens read, use shunting yard algorithm to build an AST.
//
//
// OPERAND DATA STRUCTURES
// ========================
//
// Operands include the set of all tokens, excluding:
//
//   * LEFT PAREN       (
//   * RIGHT PAREN      )
//   * COND ALWAYS      &
//   * COND SUCCESS     &&
//   * COND OR          ||
//   * LEFT REDIRECT    >
//   * RIGHT REDIRECT   <
//
// Because the early parts of the tokeniser is dealing with individual characters,
// we need a mechanism of joining tokens together.  Attacked to the Token DU are two
// useful members:
//
//   (+) - An overloaded + operator which concats tokens together, returning either a
//         brand new token, or appending the char to an existing token.  The table below
//         shows how inputs A and B are handled by this operator:
//
//         | Input Token A  | Input Token B  | Output Token     |
//         |----------------|----------------|------------------|
//         | Literal    "x" | Literal    "y" | Literal     "xy" |
//         | Pipe       "|" | Pipe       "|" | CondOr      "||" |
//         | CondAlways "&" | CondAlways "&" | CondSuccess "&&" |
//
//
//   CanConcat - A 'gatekeeper' function which just checks whether tokenA and tokenB
//               can be concatenated together.  Useful if you want to avoid writing
//               exception handler code, as the overloaded '+' above will throw if
//               given types which cannot be joined.
//
//
// STACKS
// ======
//
// During the tokenising phase, two stacks are used; one to hold OPERATORS, and another
// to hold OPERANDS.  The OPERATOR stack is straightforward, while the OPERAND stack has
// a slight complication.  The diagram below shows the two stacks, and their differences.
// Essentially, OPERATORS are always of type Token, while OPERANDS are grouped together.
//
//             INPUT:     foo && bar | baz
//
//   After tokenisation, the stacks appear:
//
//                   OPERATORS                   OPERANDS
//                +--------------+           +--------------+
//                | PIPE         |           | [bar baz]    |
//                +--------------+           +--------------+
//                | COND SUCCESS |           | [foo]        |
//                +--------------+           +--------------+
//
// Notice how the OPERANDS stack is grouped in to "commands", which are the instructions
// between OPERATORS.
//
namespace Deobfuscator

open System.Text.RegularExpressions

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


type Stack =
    | Operand
    | Operator
    | Neither


type NodeType =
    | Oper of Token
    | Command  of Token list


type Tree =
    | Empty
    | Node of value: NodeType * left: Tree * right: Tree


// ROP
// ===
//
// Trying out Railway-Oriented-Programming (ROP) for passing success|failure messages
// downstream.
//
// Read more: https://fsharpforfunandprofit.com/rop
//
//
type ErrorMessage =
    | InvalidSyntax

type ParseResult<'TSuccess, 'TMessage> =
    | Success of 'TSuccess * 'TMessage list
    | Failure of ErrorMessage


type TokeniserResult =
    | OK of Tree
    | SyntaxError


type Matcher =
    | MatchingSpecialChars
    | IgnoringSpecialChars


type TokeniserState = {
    Escape: bool
    Mode: Matcher
    LastModifiedStack: Stack
    OperandStack: Token list list
    OperatorStack: Token list
}

module Tokeniser =

    let succeed x =
        Success (x, [])

    let succeedWithMsg x msg =
        Success (x, [msg])

    let fail msg =
        Failure msg

    let bind fn result =
        match result with
        | Success (x, _) -> fn x
        | Failure f      -> Failure f


    let sameType a b = a.GetType() = b.GetType()


    let private (|ESC|QUOTE|LPAREN|RPAREN|LREDIRECT|RREDIRECT|OTHER|) str =
        match str with
        | "^"  -> ESC   str
        | "\"" -> QUOTE str
        | "("  -> LPAREN str
        | ")"  -> RPAREN str
        | "<"  -> LREDIRECT str
        | ">"  -> RREDIRECT str
        | _    -> OTHER str


    let private (|AMPERSAND|PIPE|OTHER|) str =
        match str with
        | "&" -> AMPERSAND str
        | "|" -> PIPE str
        | _   -> OTHER str


    let private (|DELIM|OTHER|) chr =
        match chr with
        | ","
        | "="
        | " "
        | ";" -> DELIM chr
        | _   -> OTHER chr


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

        let operandsGroup = ctx.OperandStack.Head

        if ctx.OperandStack.Length = 0 then
            ((token :: operandsGroup) |> catStack) :: ctx.OperandStack.Tail
        elif ctx.LastModifiedStack = Operand || ctx.LastModifiedStack = Neither then
            // When the last modified stack is the operand stack, we shove this token
            // in to the current group.
            ((token :: operandsGroup) |> catStack) :: ctx.OperandStack.Tail
        else
            // The last thing we saw was an Operator, so start a new
            // operands group...
            [token] :: ctx.OperandStack


    let rec tokeniseCmd (cmdstr: string list) (ctx: TokeniserState) =

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

        let operandStack =
            operandGroups
            |> List.map (fun grp -> (Node ((Command (grp |> List.rev)), Empty, Empty)))

        let addNode (stack: Tree list) newThing =
            if stack.Length < 2 then
                fail InvalidSyntax
            else
                let rhs = stack.[0]
                let lhs = stack.[1]
                let node = Node ((Oper newThing), lhs, rhs)
                succeed (node :: (popN stack 2))


        let rec mkAST operators (operands: Tree list) =
            match operators with
            | [] -> succeed operands.Head
            | _ ->
                let x = (addNode operands operators.Head)
                match x with
                | Success (ast, _) -> (mkAST (operators.Tail) ast)
                | Failure f -> fail InvalidSyntax

        mkAST operatorStack operandStack


    let handleResult result =
        match result with
        | Success (x, _) -> OK x
        | Failure f -> SyntaxError


    let tokenise (cmdstr: string) =

        if Regex.IsMatch(cmdstr, "^[|><&]") then
            handleResult (fail InvalidSyntax)
        else
            let state = {
                Escape = false
                Mode = MatchingSpecialChars
                LastModifiedStack = Neither
                OperandStack = [[]]; OperatorStack = []
            }

            cmdstr.ToString()
                |> Seq.toList
                |> List.map (fun ch -> ch.ToString())
                |> (fun x -> tokeniseCmd x state)
                |> (fun ctx -> makeAST ctx.OperatorStack ctx.OperandStack)
                |> handleResult
