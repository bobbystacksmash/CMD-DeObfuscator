namespace Deobfuscator

open Deobfuscator.Expander.ExpanderWithCommandExtensions
open Deobfuscator.Tokeniser

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
    | LookingForInternalInstruction
    | LookingForExternalInstruction
    | LookingForQuotedExternalInstruction
    | LookingForArguments


type ExecutionContext = {
    Variables: Map<string,string>
    Tokens: Token list
    Commands: Command list
    ReadMode: BuildingCommand
}


type InterpreterStatus =
    | Complete
    | NoInputTokens
    | NoFurtherInputTokens
    | CannotFindClosingQuote
    | FoundClosingQuote
    | UnrecognisedInternalCommand

module Interpreter =

    let private (|DELIMITER|QUOTE|TOKEN|) token =
        match token with
        | Delimiter x -> DELIMITER
        | Quote x -> QUOTE
        | _ -> TOKEN


    let private ignoreDelimiter readMode =
        match readMode with
        | LookingForInstruction
        | LookingForExternalInstruction
        | LookingForInternalInstruction
        | LookingForQuotedExternalInstruction -> true
        | _ -> false


    let rec private findQuotedPart tokens accum =
        match tokens with
        | [] -> Error (CannotFindClosingQuote, [], [])
        | head :: rest ->
            match head with
            | Quote qt ->
                Ok (FoundClosingQuote, (accum |> List.rev), rest)
            | _ ->
                findQuotedPart rest (head :: accum)


    let rec private runCMD ctx =
        match ctx.Tokens with
        | [] when ctx.Commands.Length > 0 ->
            Ok (NoFurtherInputTokens, ctx)

        | [] ->
            Error (NoInputTokens, ctx)

        | head :: rest ->
            match head with
            | DELIMITER when ignoreDelimiter ctx.ReadMode ->
                runCMD {ctx with Tokens = rest}

            | QUOTE when ctx.ReadMode = LookingForInstruction ->
                runCMD {ctx with Tokens = rest; ReadMode = LookingForQuotedExternalInstruction}

            | TOKEN when ctx.ReadMode = LookingForInstruction ->
                runCMD {ctx with Tokens = rest; ReadMode = LookingForInternalInstruction}

            | _ when ctx.ReadMode = LookingForQuotedExternalInstruction ->
                match findQuotedPart ctx.Tokens [] with
                | Ok (_, quotedPart, remaining) ->
                    printfn "Got quoted part -> %A" quotedPart
                    printfn "Remaining       -> %A" remaining
                    Ok (NoFurtherInputTokens, ctx)

                | Error (reason, _, _) ->
                    Error (reason, ctx)

//
// PARENTHESIS
//
//   (echo foo)                       [prints "foo"]
//   echo foo (calc)                  [prints "foo (calc)"]
//   (echo foo & calc)                [prints "foo" ; launches calc]
//   echo foo (notepad & calc         [prints "foo (notepad" ; launches calc]
//
//
// CONDITIONALS
//
//   if defined comspec echo foo & echo bar
//   [prints "foo"; prints "bar"]
//   IF (defined comspec) THEN
//     echo foo
//     echo bar
//   ENDIF
//
//   if defined xxxspec echo foo & echo bar
//   [no output]
//   IF (defined xxxspec) THEN
//     echo foo
//     echo bar
//   ENDIF
//
//   (if defined comspec) echo foo & echo bar
//   [Syntax error: ')' is unexpected]
//
//   (if defined comspec echo) foo & echo bar
//   [Syntax error: 'foo' was unexpected]
//
//   (if defined comspec echo foo) & echo bar
//   [prints "foo" ; prints "bar"]
//   IF (defined comspec) THEN
//     echo foo
//   ENDIF
//   echo bar
//
//   (if defined XXXspec echo foo) & echo bar
//   [prints "bar"]
//   IF (defined XXXspec) THEN
//     echo foo
//   ENDIF
//   echo bar
//
//   if defined comspec (echo foo) & echo bar
//   [prints "foo" ; prints "bar"]
//   IF (defined comspec) THEN
//     (echo foo)
//   ENDIF
//   echo bar
//
//   if defined comspec (echo foo) else (echo bar)
//   [prints "foo"]
//   IF (defined comspec) THEN
//     (echo foo)
//   ELSE
//     (echo bar)
//   ENDIF
//
//   if defined XXXspec (echo foo) else (echo bar)
//   [prints "bar"]
//   IF (defined XXXspec) THEN
//     (echo foo)
//   ELSE
//     (echo bar)
//   ENDIF
//
//   * For loops?
//   * Conditionals?
//



    let execute vars cmdstr =
        let ctx = {
            Variables = vars
            Tokens    = tokenise (expand cmdstr vars)
            Commands  = []
            ReadMode  = LookingForInstruction
        }

        printfn "==================="
        printfn "> %A" cmdstr
        printfn "%A" (tokenise (expand cmdstr vars))
        printfn "==================="
        runCMD ctx



//
// TEST CASES
//
//   cmd/ccalc                    [launches calc]
//   [Literal (Sym "cmd/ccalc")]
//
//   "cmd"/ccalc                  [launches calc]
//   [Quote (Sym """); Literal (Sym "cmd"); Quote (Sym """); Literal (Sym "/ccalc")]
//
//   "cmd /ccalc                  [launches calc]
//   [Quote (Sym """); Literal (Sym "cmd/ccalc")]
//
//   cmd;/c;calc                  [launches calc]
//   [Literal (Sym "cmd"); Delimiter (Sym ";"); Literal (Sym "/c"); Delimiter (Sym ";"); Literal (Sym "calc")]
//
//   cmd,/c,calc                  [launched calc]
//   [Literal (Symbol "cmd"); Delimiter (Symbol ","); Literal (Symbol "/c"); Delimiter (Symbol ","); Literal (Symbol "calc")]
//
//   "cmd/ccalc                   [crashes: cannot find path specified]
//   [Quote (Symbol """); Literal (Symbol "cmd/ccalc")]
//
//
//
//   "echo foo                    [crashes: not a recognised command]
//   Quote (Symbol """); Literal (Symbol "echo foo")
//
//   "echo foo"                   [crashes: "echo foo" is not recognised as a command]
//
//
//   "echo" foo                   [crashes: "echo" not regognised command]
//
//   echofoo                      [crashes: unknown program "echofoo"]
//
//
//   echo "foo                    [prints: "foo]
//
//
//   echo foo (echo bar)          [prints: foo (echo bar)]
//
//   echo/a                       [prints: a]
//
//   echo/c                       [prints: c]
//
//   echo /c                      [prints: /c]
//
//   echo foo (bar & baz)         [prints: foo (bar] ; [crashes: 'baz' is unknown command]
//
//
//   "" calc                      [crashes: unregocnised command ""]
//
//
//   if "a" == "a" echo "nice"                 [prints: "nice"]
//   if "a" == "b" echo "nice" & echo "foo"    [prints: "" (NOTHING)]
