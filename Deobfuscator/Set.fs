namespace Deobfuscator.Instruction

open Deobfuscator.DomainTypes

open System.Text.RegularExpressions

module CommandSet =

    let private cleanSetExpr expr =

        let trimQuotes expr =
            if Regex.IsMatch("^\".+\"$", expr) then
                Regex.Replace(expr, "^\"|\"$", "")
            else
                expr

        trimQuotes expr


    let private (|SetExpr|_|) expr =
        let cleaned = cleanSetExpr expr
        let matched = Regex.Match(cleaned, "^([^=]+)=(.+)$")

        if matched.Success then
            printfn "SET EXPR -> %A / %A" matched.Groups.[1] matched.Groups.[2]
            Some (matched.Groups.[1].ToString().ToUpper(), matched.Groups.[2].ToString())
        else
            None


    let CmdSet (ctx: CommandContext) args argstr =
        match argstr with
        | SetExpr (varName, varValue) ->
            let log  = sprintf "SET %s=%s" varName varValue
            let vars = ctx.EnvVars.Add(varName, varValue)
            Ok (CommandSuccess, {ctx with Log = log :: ctx.Log; EnvVars = vars})

        | _ ->
            let errReason = MalformedArgumentExpression (sprintf "Cannot parse assignment expression: %A" argstr)
            Error (errReason, ctx)
