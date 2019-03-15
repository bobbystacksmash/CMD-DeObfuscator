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


type Instruction = Instruction of CommandToken
type Command = {
    Instruction: Instruction
    Arguments: Token list
}


type CommandBlock = {
    CurrentCommand: Command option
    RemainingTokens: Token list option
}


type IdentifyCommandMode =
    | LookingForCommandToken
    | LookingForGroupTerminal
    | LookingForCommandTerminator


type IdentifyCommandResult =
    | NoMoreTokens


type CommandIdentifier = {
    ReadMode: IdentifyCommandMode
    ParenCounter: int
}

type EnvironmentFlags =
    | EnableCommandExtensions
    | DisableCommandExtensions
    | EnableDelayedExpansion
    | DisableDelayedExpansion


type CommandEnvironment = {
    Flags: EnvironmentFlags list
    Vars: Map<string,string>
    Cmdstr: string
    Commands: Command list // holds each of the commands that were run
}


// Represents CMD.EXE running in command-line mode
// ===============================================
//
//
module CommandInterpreter =

    let toLookingForGTerm ctx =
        {ctx with ReadMode = LookingForGroupTerminal}



    let rec exec ctx tokens =
        match tokens with
        | head :: rest ->
            match head with
            | LeftParen _ when ctx.ReadMode = LookingForCommandToken ->
                



    let identifyCommand (tokens: Token list) =

        let ctx = {
            ReadMode = LookingForCommandToken
            ParenCounter = 0
        }

        exec ctx tokens

// C:\Windows\System32\cmd.exe /V /c set %foo%=bar&&set %abc%=def...








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
