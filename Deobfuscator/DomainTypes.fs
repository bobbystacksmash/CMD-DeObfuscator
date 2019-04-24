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


type ParseResult =
    | Cmd of Token list
    | Op of Operator




(* FOR loop types *)

type ForLoopHeaderParseStatuses =
    | LooksGood
    | FeatureNotImplemented
    | FlagOrLoopVarExpected of string
    | SkipStringNotNumeric of string
    | UnknownFlag of string
    | HeaderListIsEmpty of string
    | HeaderListIsTooLong of string
    | MissingInKeyword of string
    | MissingVariableIdentifier of string
    | VariableIdentifierNotValid of string
    | CannotFindForInKeyword of string
    | LoopVariableIsNotValid of string
    | UnrecognisedParseKeyword of string
    | ExpectedParseKeywordValue of string
    | InvalidKeyword of string
    | KeywordTokensIsInvalid of string
    | KeywordEolTooManyChars of string
    | KeywordSkipCannotBeZero of string
    | KeywordSkipValueIsNotNumeric of string
    | KeywordSkipNumericConversionFailed of string
    | TooManyWildcardsUsedInTokensExpr of string
    | WildcardNotAllowedHereInTokensExpr of string


type ForTokenExpr = {
    Cols: int list
    UseWildcard: bool
}

type ForLoopParsingArgs = {
    EOL: char
    Skip: int
    Delims: char list
    Tokens: ForTokenExpr
    UseBackq: bool
}