namespace Deobfuscator.Expander

open System.Text.RegularExpressions

module ExpanderWithCommandExtensions =

    type VarnameContext = {
        Name: string
        Rest: string
        Failed: bool
    }

    type private EnvVars = Map<string,string>


    type private FindReplaceExpression = {
        Find: string
        Replace: string
    }


    type private SubstringExpression = {
        Start: int
        Length: int
    }


    let private (|FindReplace|_|) expression =
        let m = Regex.Match(expression, "^:([^=]+)[=]([^=]+)?$")
        if m.Success then
            Some ({ Find = m.Groups.[1].Value; Replace = m.Groups.[2].Value})
        else
            None


    let private (|RangeDec|RangeHex|RangeOct|RangeInvalid|) range =
        if Regex.IsMatch(range, "^[-+]?[0]+$") || Regex.IsMatch(range, "^[-+]?[1-9][0-9]*$") then
            RangeDec
        elif Regex.IsMatch(range, "^[-+]?0x[a-f0-9]+$") then
            RangeHex
        elif Regex.IsMatch(range, "^[-+]?0[0-7]+") then
            RangeOct
        else
            RangeInvalid


    let private (|Positive|Negative|) numstr =
        if Regex.IsMatch(numstr, "^[-]") then Negative
        elif Regex.IsMatch(numstr, "^[+]") then Positive
        else Positive


    let private rangeToInt32 range regex (numBase: int32) =
        let rangeInt = System.Convert.ToInt32(Regex.Replace(range, regex, ""), numBase)
        match range with
        | Negative -> rangeInt * -1
        | _ -> rangeInt


    let private hexRangeToInt32 range =
        rangeToInt32 range "^[-+]?0x" 16


    let private octRangeToInt32 range =
        rangeToInt32 range "^[-+]?0" 8


    let private decRangeToInt32 range =
        rangeToInt32 range "^[-+]?" 10


    let private (|Substring|_|) (expression: string) =

        let expNoSpaces = Regex.Replace(expression, "\s*", "")
        let m = Regex.Match(expNoSpaces, "^:~([^,]+)(?:,(.+)$)?")
        if not m.Success then
            None
        else
            let startStr = m.Groups.[1].Value
            let startNum = match startStr with
                           | RangeDec -> decRangeToInt32 startStr
                           | RangeHex -> hexRangeToInt32 startStr
                           | RangeOct -> octRangeToInt32 startStr
                           | _        -> 0

            let lengthStr = m.Groups.[2].Value
            let lengthNum = match lengthStr with
                            | RangeDec -> decRangeToInt32 lengthStr
                            | RangeHex -> hexRangeToInt32 lengthStr
                            | RangeOct -> octRangeToInt32 lengthStr
                            | _        -> 0

            Some { Start = startNum; Length = lengthNum }


    let rec private parseCmdExtExpr (ctx: VarnameContext) (cmd: char list) =
        match cmd with
        | head :: rest ->
            match head with
            | '=' -> {ctx with Failed = true}
            | ':' when rest.Length = 0 -> {ctx with Name = (ctx.Name + head.ToString())}
            | ':' -> {ctx with Rest = (":" + (String.concat "" <| List.map string rest))}
            | _ -> parseCmdExtExpr {ctx with Name = (ctx.Name + head.ToString())} rest
        | _ -> ctx


    let private (|ExpansionExpression|Literal|) str =
        let m = Regex.Match(str, "%([^%]+)%")
        if m.Success then
            ExpansionExpression(m.Groups.[1].Value)
        else
            Literal(str)

    let private varnameExists (vars: Map<string,string>) (varname: string) =
        Map.containsKey (varname.ToUpper()) vars


    let private varValue (vars: Map<string,string>) (varname: string) =
        let key = varname.ToUpper()
        vars.[key]


    let private doSubstring (value: string) (substr: SubstringExpression) =
        // TODO: finish this implementation.
        value.Substring(substr.Start, substr.Length)


    let private doFindReplace (value: string) (findreplace: FindReplaceExpression) =
        "THIS IS A REPLACED VALUE"

    let rec expandEnvironmentVariable (cmdstr: string) (vars: Map<string,string>) =

        let defaultVarnameCtx = {
            Name = ""
            Rest = ""
            Failed = false
        }

        let expr = parseCmdExtExpr defaultVarnameCtx (cmdstr.ToCharArray() |> Seq.toList)

        match expr.Rest with
        | Substring substr when (varnameExists vars expr.Name) ->
            doSubstring (varValue vars expr.Name) substr

        | FindReplace findreplace when (varnameExists vars expr.Name) ->
            doFindReplace (varValue vars expr.Name) findreplace

        | _ when (varnameExists vars expr.Name) ->
            varValue vars expr.Name

        | _ ->
            // The assumption that there's always a colon may cause problems later.
            "%" + expr.Name + expr.Rest + "%"


    let expand cmdstr vars =
        Regex.Split(cmdstr, "(%[^%]+%)")
            |> Seq.map(fun x ->
                match x with
                | ExpansionExpression exp -> expandEnvironmentVariable exp vars
                | Literal str -> str)
            |> String.concat ""

