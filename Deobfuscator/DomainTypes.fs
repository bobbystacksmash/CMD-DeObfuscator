namespace Deobfuscator.DomainTypes

type InterpreterStatus =
    | Halted
    | CommandError
    | CannotIdentifyInstruction
    | CommandBlockIsEmpty

type CommandContext = {
    EnvVars: Map<string,string>
    StdOut: string
    StdIn: string
    Log: string list
    InputCmd: string
}

type CommandFailureReason =
    | MalformedArgumentExpression of string

type CommandStatus =
    | CommandSuccess
    | CommandFailure of CommandFailureReason

type Operator =
    | Pipe
    | LeftRedirect
    | RightRedirect
    | CondOr
    | CondAlways
    | CondSuccess
    | OpenParen
    | CloseParen

type Token =
    | Literal of string
    | Delimiter of string
    static member (+) (a: Token, b: Token) =
        let strA =
            match a with
            | Literal lit -> lit
            | Delimiter delim -> delim

        let strB =
            match b with
            | Literal lit -> lit
            | Delimiter delim -> delim

        Literal(strA + strB)


type Ast =
    | Cmd of Token list
    | Op of Operator

