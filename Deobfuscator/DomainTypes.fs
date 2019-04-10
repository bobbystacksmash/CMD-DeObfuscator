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


(* FOR loop types *)
type ForLoopHeaderParseStatuses =
    | LooksGood of string option
    | FeatureNotImplemented of string option
    | FlagOrLoopVarExpected of string option
    | SkipStringNotNumeric of string option
    | UnknownFlag of string
    | HeaderListIsEmpty of string option
    | HeaderListIsTooLong of string option
    | MissingInKeyword of string option
    | MissingVariableIdentifier of string option
    | VariableIdentifierNotValid of string option
    | CannotFindForInKeyword of string option
    | LoopVariableIsNotValid of string option
    | UnrecognisedParseKeyword of string option
    | KeywordSkipValueIsNotNumeric of string option
    | KeywordSkipNumericConversionFailed of string option

type ForLoopParsingKeywords = {
    EOL: string
    Skip: string
    Delims: char list
    Tokens: int list
    WildcardVar: bool
    UseBackq: bool
}