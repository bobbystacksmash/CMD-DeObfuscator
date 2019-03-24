namespace Deobfuscator

open Deobfuscator.Expander.ExpanderWithCommandExtensions
open Deobfuscator.Tokeniser

// Internal commands, found at:
// - https://ss64.com/nt/syntax-internal.html
//
type InternalCommand =
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

type ExternalCommand =
    | ExternalCommand of string


type CommandContext = {
    EnvVars: Map<string,string>
    StdOut: string
    InputCmd: string
}

type CommandStatus =
    | Success
    | Failure

type Command =
    | ICmd of InternalCommand
    | ECmd of ExternalCommand

type InstructionExpression = {
    Instruction: Command
    Args: Token list
}

type InterpreterStatus =
    | Halted
    | CommandError
    | CommandBlockIsEmpty

type InterpreterResult =
    | Status of InterpreterStatus * CommandContext


module Interpreter =

    let rec trimLeadingDelims lst =
        match lst with
        | [] -> lst
        | head :: rest ->
            match head with
            | Delimiter _ -> trimLeadingDelims rest
            | _ -> lst


    let trimTrailingDelims lst =
        lst |> List.rev |> trimLeadingDelims |> List.rev


    let trimDelimiters lst =
        trimLeadingDelims lst |> trimTrailingDelims


    let tokenToString tok =
        match tok with
        | Delimiter d -> d
        | Literal   l -> l


    let private cmdExternal ctx args =
        Error (Failure, ctx)


    let private cmdEcho ctx args =

        let output = (trimDelimiters args) |> List.map tokenToString |> List.fold (+) ""
        printfn "@ECHO %s" output
        Ok (Success, {ctx with StdOut = output})


    let private identifyCommand cmd args =
        let (Literal strval) = cmd
        match strval.ToUpper() with
        | "ECHO" -> (fun (ctx) -> cmdEcho ctx args)
        | _ -> (fun (ctx) -> cmdExternal ctx args)



    let tryIdentifyCommand cmd =
        match (cmd |> trimDelimiters) with
        | [] ->
            Error CommandBlockIsEmpty

        | head :: rest ->
            Ok (identifyCommand head rest)


    let rec private dispatchCommand ctx cmd =

        let cmdfunc = (tryIdentifyCommand cmd)

        match (tryIdentifyCommand cmd) with
        | Ok cmdfunc ->
            (* Run the command! *)
            match (cmdfunc ctx) with
            | Ok (_, newctx) ->
                (Halted, newctx)

            | Error (reason, errctx) ->
                (CommandError, errctx)

        | Error reason ->
            (CommandError, ctx)



    let rec private astWalk (ctx: CommandContext) ast =
        match ast with
        | [] -> (Halted, ctx)
        | head :: rest ->
            match head with
            | Op CondAlways ->
                astWalk ctx rest

            | Cmd cmd ->
                dispatchCommand ctx cmd



    let evaluate cmdctx =
        let expanded = expand (cmdctx.InputCmd.Trim()) cmdctx.EnvVars
        match parse expanded with
        | Ok ast ->
            printfn "-- start interpreting AST --"
            Ok (astWalk cmdctx ast)
        | Error reason ->
            printfn "Error parsing input command -> %A" reason
            Error reason

//
// PARENTHESIS
//
//   (echo foo)                       [prints "foo"]
//   echo foo (calc)                  [prints "foo (calc)"]
//   (echo foo & calc)                [prints "foo" ; launches calc]
//   echo foo (notepad & calc         [prints "foo (notepad" ; launches calc]
//   )echo foo                        [Syntax error: ')echo' is not recognised]
//   ) echo foo                       [prints "" - everything after ')' is ignored]
//   )&echo foo                       [prints "" - everything after ')' is ignored]
//   ),echo foo                       [prints "" - everything after ')' is ignored]
//   );echo foo                       [prints "" - everything after ')' is ignored]
//   )/echo foo                       [Syntax error: ')' is not a recognised command]
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
