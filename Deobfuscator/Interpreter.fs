namespace Deobfuscator

open Deobfuscator.Expander

// Built-in list, found at:
//  - https://ss64.com/nt/commands.html
//
// Will only implement a subset of these commands.
type CommandToken =
    | CALL
    | CLS
    | COLOR
    | ECHO
    | ENDLOCAL
    | EXIT
    | FOR
    | IF
    | PROMPT
    | PAUSE
    | REM
    | SET
    | SETLOCAL
    | SHIFT
    | START
    | TITLE
    | VOL
    | ASSOC
    | CD
    | COPY
    | DIR
    | DEL
    | ERASE
    | FTYPE
    | MOVE
    | POPD
    | REN
    | RD
    | TYPE
    | DATE
    | TIME
    | VER
    | VERIFY
    | OTHER of Token

// Given a sequence of Tokens, such as:
//
//   [echo; foo; bar]
//
// `echo'    = Instruction
// `foo bar' = Arguments
//
type Instruction = Instruction of CommandToken
type Command = {
    Instruction: Instruction
    Arguments: Token list
}


type CommandBlock = {
    currentCommand: Command
    remainingTokens: Token list
}


type BuildingCommand =
    | LookingForInstruction
    | LookingForArguments


type InterpreterError =
    | TokenListIsEmpty
    | UnexpectedCommandDelimiter


module Interpreter =

    let private (|DELIMITER|CMDTOKEN|) token =
        match token with
        | CondAlways tok
        | CondSuccess tok
        | CondOr tok
        | Pipe tok -> DELIMITER
        | _ -> CMDTOKEN


    let rec private findNextCommandBlock (tokens: Token list) (accum: Token list) readMode =
        match tokens with
        | [] when accum.Length = 0 -> Error TokenListIsEmpty
        | head :: rest ->
            match head with
            | CMDTOKEN when readMode = LookingForInstruction ->
                findNextCommandBlock rest (head :: accum) LookingForArguments

            | CMDTOKEN when readMode = LookingForArguments ->
                findNextCommandBlock rest (head :: accum) LookingForArguments

            | DELIMITER when readMode = LookingForInstruction ->
                Error UnexpectedCommandDelimiter

            | DELIMITER ->
                // This is the point that we break the recursive loop and return
                // the CommandBlock.







    let execute (tokens: Token list) =
        // TODO: filter tokens such as 'Quote' and 'Delimiter' from token stream.
