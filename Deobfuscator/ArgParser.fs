namespace Deobfuscator

open Deobfuscator.Parser

type private ArgParseState = {
    Mode: ReadMode
    Escape: bool
    Input: char list
    Output: string list
    StartNew: bool
}

module ArgumentParser =

    let private pushStr ctx ch =
        let str = ch.ToString()
        match ctx.Output with
        | [] -> {ctx with Output = [str]}
        | head :: _ ->
            match head with
            | _ when ctx.StartNew ->
                {ctx with Output = str :: ctx.Output ; StartNew = false}

            | _ ->
                {ctx with Output = (ctx.Output.Head + str) :: ctx.Output.Tail}


    let rec private parseArgstr (ctx: ArgParseState) =
        match ctx.Input with
        | [] -> ctx.Output |> List.rev
        | head :: rest ->
            match head with
            | _ when ctx.Escape ->
                parseArgstr {(pushStr ctx head) with Escape = false; Input = rest}

            | '\\' when ctx.Mode = MatchSpecial ->
                parseArgstr {ctx with Escape = true}

            | '"' when ctx.Mode = IgnoreSpecial ->
                parseArgstr {ctx with Mode = MatchSpecial; Input = rest}

            | '"' when ctx.Mode = MatchSpecial ->
                parseArgstr {ctx with Mode = IgnoreSpecial; Input = rest}

            | ' ' when ctx.Mode = MatchSpecial ->
                parseArgstr {ctx with StartNew = true ; Input = rest}

            | _ ->
                parseArgstr {(pushStr ctx head) with Input = rest}



    let argparse (argstr: string) =

        let ctx = {
            Mode   = MatchSpecial
            StartNew  = false
            Escape = false
            Input  = List.ofSeq argstr
            Output = []
        }
        parseArgstr ctx

