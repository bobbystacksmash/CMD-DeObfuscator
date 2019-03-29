namespace Deobfuscator

module ArgumentParser =

    type private ReadMode =
        | InterpretSpecialChars
        | IgnoreSpecialChars

    type private ArgvParser = {
        Argv: string list
        Mode: ReadMode
    }

    type private Argv = {
        Arguments: string list
        CurrentArg: string
    }

    let private (|EQUOTE|_|) lst =
        match lst with
        | head :: rest ->
            match head with
            | '"' -> Some (head, rest)
            | _ -> None
        | _ -> None


    let rec private buildArgv chars ctx argv =
        match chars with
        | [] -> (argv.CurrentArg :: argv.Arguments) |> List.rev
        | head :: rest ->
            match head with

            | '\\' ->
                match rest with
                | EQUOTE (lookahead, lookaheadRest) ->
                    buildArgv lookaheadRest ctx {argv with CurrentArg = argv.CurrentArg + "\""}

                | _ ->
                    buildArgv rest ctx {argv with CurrentArg = "\\" + (head.ToString())}

            | '"' when ctx.Mode = InterpretSpecialChars ->
                buildArgv rest {ctx with Mode = IgnoreSpecialChars} argv

            | '"' when ctx.Mode = IgnoreSpecialChars ->
                buildArgv rest {ctx with Mode = InterpretSpecialChars} argv

            | ' ' when ctx.Mode = InterpretSpecialChars ->
                let newArgv = {
                    Arguments = (argv.CurrentArg :: argv.Arguments)
                    CurrentArg = ""
                }
                buildArgv rest ctx newArgv

            | _ ->
                buildArgv rest ctx {argv with CurrentArg = argv.CurrentArg + (head.ToString())}


    let parseArgs argstr =
        let ctx = {
            Mode = InterpretSpecialChars
            Argv = []
        }

        let args = {
            Arguments = []
            CurrentArg = ""
        }
        buildArgv (List.ofSeq argstr) ctx args