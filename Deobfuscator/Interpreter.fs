namespace Deobfuscator

open Deobfuscator.Expander

// Internal commands, found at:
// - https://ss64.com/nt/syntax-internal.html
//
type CommandToken =
    | Assoc
    | Break
    | Call
    | Cd
    | Chdir
    | Cls
    | Color
    | Copy
    | Date
    | Del
    | Dir
    | Dpath
    | Echo
    | EndLocal
    | Erase
    | Exit
    | For
    | Ftype
    | Goto
    | If
    | Keys
    | Md
    | Mkdir
    | Mklink
    | Move
    | Path
    | Pause
    | Popd
    | Prompt
    | Pushd
    | Rem
    | Ren
    | Rename
    | Rd
    | Rmdir
    | Set
    | SetLocal
    | Shift
    | Start
    | Time
    | Title
    | Type
    | Ver
    | Verify
    | Vol
    | ExternalCommand of Token

// Given a list of Tokens, such as:
//
//   [echo, foo, bar]
//
// `echo'    = Instruction
// `foo bar' = Arguments
//
type Instruction = Instruction of CommandToken
type Command = {
    Instruction: Instruction
    Arguments: Token list
}

//
// TODO: See "Phase 7) Execute" from the following SO question for details
//       about command identidication.
//
//       https://stackoverflow.com/questions/4094699/how-does-the-windows-command-interpreter-cmd-exe-parse-scripts/7970912#7970912
//

type CommandBlock = {
    CurrentCommand: Command
    RemainingTokens: Token list
}


type BuildingCommand =
    | LookingForInstruction
    | LookingForArguments


type InterpreterError =
    | TokenListIsEmpty
    | UnexpectedCommandDelimiter


module Interpreter =

    let private (|CMDDELIMITER|CMDTOKEN|) token =
        match token with
        | CondAlways tok
        | CondSuccess tok
        | CondOr tok
        | Pipe tok -> CMDDELIMITER
        | _ -> CMDTOKEN


    let rec private findNextCommandTokens (tokens: Token list) (accum: Token list) readMode =
        match tokens with
        | [] when accum.Length = 0 -> Error TokenListIsEmpty
        | [] when accum.Length > 0 -> Ok ((accum |> List.rev), [])
        | head :: rest ->
            match head with
            | CMDTOKEN when readMode = LookingForInstruction ->
                findNextCommandTokens rest (head :: accum) LookingForArguments

            | CMDTOKEN when readMode = LookingForArguments ->
                findNextCommandTokens rest (head :: accum) LookingForArguments

            | CMDDELIMITER when readMode = LookingForInstruction ->
                Error UnexpectedCommandDelimiter

            | CMDDELIMITER ->
                // This is the point that we break the recursive loop and return
                // the CommandBlock.
                Ok ((accum |> List.rev), rest)


    let private skipToken token =
        match token with
        | Quote tok
        | Delimiter tok -> false
        | _ -> true


    let execute (tokens: Token list) =

        let filtered = List.filter skipToken tokens

        // TODO: filter tokens such as 'Quote' and 'Delimiter' from token stream.
        match (findNextCommandTokens tokens [] LookingForInstruction) with
        | Ok (cmdTokens, remaining) ->
            printfn "CMD TOKENS -> %A" cmdTokens
            printfn "Remaining  -> %A" remaining
            cmdTokens

        | Error msg ->
            printfn "Error... %A" msg
            tokens

//
// TEST CASES
//
//   cmd/ccalc                    [launches calc]
//   "cmd"/ccalc                  [launches calc]
//   "cmd /ccalc                  [launches calc]
//   "cmd/ccalc                   [crashes: cannot find path specified]
//   "echo foo                    [crashes: not a recognised command]
//   echo "foo                    [prints: "foo]
//   echo foo (echo bar)          [prints: foo (echo bar)]
//   echo/a                       [prints: a]
//   echo/c                       [prints: c]
//   echo /c                      [prints: /c]
//   echo foo (bar & baz)         [prints: foo (bar] ; [crashes: 'baz' is unknown command]
//   "echo foo"                   [crashes: "echo foo" is not recognised as a command]
//   "echo" foo                   [prints: foo]
//
//   if "a" == "a" echo "nice"                 [prints: "nice"]
//   if "a" == "b" echo "nice" & echo "foo"    [prints: "" (NOTHING)]
