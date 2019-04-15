namespace Deobfuscator.For

open Deobfuscator.DomainTypes
open System.Text.RegularExpressions

type ParseArgsMode =
    | LookingForKey
    | LookingForValue


type ForFParseStatus = {
    Mode: ParseArgsMode
    CurrKey: string
    CurrValue: string
}

type private FindExtractStatus =
    | ExtractSuccess of string * string
    | NoMatch


type private TokenColumn =
    | Column of int
    | Wildcard
    | Invalid of string


module ForFileParser =
    // ReactOS For /F Implementation:
    //   https://doxygen.reactos.org/d0/d06/for_8c_source.html#l00129

    let private (|Delims|Skip|EOL|Tokens|Usebackq|Useback|Unrecognised|) (str: string) =
        match str.ToLower() with
        | "delims"   -> Delims
        | "skip"     -> Skip
        | "eol"      -> EOL
        | "tokens"   -> Tokens
        | "useback"  -> Useback // Undocumented!
        | "usebackq" -> Usebackq

        | _ -> Unrecognised str

    let private (|SkipNumBase|Unknown|) str =

        if Regex.IsMatch(str, @"^0x[a-f0-9]+$", RegexOptions.IgnoreCase)
        then
            SkipNumBase 16
        elif Regex.IsMatch(str, "^0[0-7]+$")
        then
            SkipNumBase 8
        elif Regex.IsMatch(str, "^\d+$")
        then
            SkipNumBase 10
        else
            Unknown


    let private (|Number|NaN|) str =
        match str with
        | Unknown ->
            NaN
        | SkipNumBase numBase ->
            try
                Number (System.Convert.ToInt32(str, numBase))
            with
                _ ->
                    NaN


    let private getValue (chars: string list) delim =
        let rec seeker chars accum =
            match chars with
            | [] -> (accum, [])
            | head :: rest ->
                match head with
                | _ when head = delim -> (accum, rest)
                | _ ->
                    seeker rest (accum + head)
        seeker chars ""


    let private tryMatchSkip (chars: string list) =
        // Skips N lines from the input.  N can be encoded
        // either as decimal, hex, or octal.
        let (value, rest) = getValue chars " "
        printfn "SKIP -----> %A / %A" value rest
        printfn "tryMatchSkip (value, rest) = %A, %A" value rest
        match value with
        | NaN ->
            let msg = sprintf "Expected FOR /F 'skip=' value (%s) to be numeric." value
            Error (KeywordSkipValueIsNotNumeric msg)
        | Number num -> Ok (num, rest)


    let private (|TokenRange|WildcardRange|Col|WildcardCol|WC|NotValid|) expr =
        let m = Regex.Match(expr, "^(\d+)-(\d+)([*])?$")
        if m.Success
        then
            let rangeStart = m.Groups.[1].Value |> int
            let rangeEnd   = m.Groups.[2].Value |> int

            if m.Groups.Count = 4
            then
                WildcardRange [rangeStart .. rangeEnd]
            else
                TokenRange [rangeStart .. rangeEnd]
        elif Regex.IsMatch(expr, "^\d+$")
        then
            Col (expr |> int)

        elif Regex.IsMatch(expr, "^\d[*]$")
        then
            WildcardCol (expr.Replace("*", "") |> int)
        elif expr = "*" then
            WC
        else
            NotValid


    let private expandTokenRanges expr =
        match expr with
        | TokenRange range
        | WildcardRange range ->
            range |> List.map (fun x -> Column(x))

        | Col col
        | WildcardCol col ->
            [Column col]

        | WC ->
            [Wildcard]

        | NotValid ->
            [Invalid expr]


    let private (|ValidTokenExpr|_|) (exprs: TokenColumn list) =
        let numWildcards =
            exprs
            |> List.filter (fun tc ->
                match tc with
                | Wildcard _ -> true
                | _ -> false)

        let columns =
            exprs
            |> List.map (fun tc ->
                match tc with
                | Column col -> col
                | _ -> -1)
            |> List.filter (fun col -> col >= 0)
            |> set
            |> List.ofSeq

        // Wildcard validation checks...
        if numWildcards.Length > 1 then
            None
        elif numWildcards.Length = 0 then
            Some (ValidTokenExpr { Cols = columns; UseWildcard = false })
        else
            // Check wildcards only appear at the end.
            match exprs |> List.rev with
            | [] -> Some (ValidTokenExpr { Cols = columns; UseWildcard = false})
            | last :: _ ->
                match last with
                | Wildcard -> Some (ValidTokenExpr { Cols = columns; UseWildcard = true })
                | _ -> None


    let private tryParseTokenExpression (chars: string list) =
        (*
            tokens=2,4,6 will cause the second, fourth and sixth items on each line to be processed.
            tokens=2-6   will cause the second, third, fourth, fifth and sixth items on each line to be processed.
            tokens=*     will cause all items on each line to be processed.
            tokens=3*    will process the third token and the 4th + all subsequent items,
                         this can also be written as tokens=3,*
            tokens=1-1   will cause the first item to be processed.
            tokens=2-1   seems to set the vars to ""
            tokens=2-1,1,2 will set var1 and var2 appropriately.

            tokens=1,2*,3 throws an error - ',3' unexpected at this time.

            The numbers specified in `tokens=' are automatically sorted,
            for example: `tokens=5,7,1-3' and `tokens=1,2,3,5,7' produce
            the same result.
        *)
        let (value, rest) = getValue chars " "

        let columns =
            value.Split [|','|]
            |> List.ofSeq
            |> List.collect expandTokenRanges

        match columns with
        | ValidTokenExpr tokensExpr ->
            Ok (tokensExpr, rest)
        | _ ->
            Error FeatureNotImplemented


    let private resetStatus status =
        {status with CurrKey = ""; Mode = LookingForKey}


    let private isUseBackq (str: string) =
        Regex.IsMatch(str.ToLower(), "^usebackq?$")


    let rec private keyValueMatcher chars (args: ForLoopParsingArgs) status =
        match chars with
        | [] when status.Mode = LookingForKey && (isUseBackq status.CurrKey) ->
            Ok {args with UseBackq = true}

        | [] ->
            Ok args (* TODO: figure out when this is an error *)

        | head :: rest ->
            match status.Mode with
            | LookingForKey ->
                match head with
                | "=" -> (* TODO: special case is 'usebackq*)
                    // A space represents the end of a key, so switch modes.
                    keyValueMatcher rest args {status with Mode = LookingForValue}

                | " " when (isUseBackq status.CurrKey) ->
                    keyValueMatcher rest {args with UseBackq = true} (resetStatus status)

                | " " ->
                    // TODO: this will likely cause problems for us later.
                    keyValueMatcher rest args {status with CurrKey = ""}

                | _ ->
                    keyValueMatcher rest args {status with CurrKey = status.CurrKey + head}

            | LookingForValue ->
                match status.CurrKey with
                | EOL ->
                    match head with
                    | " " when rest.Length = 0 ->
                        keyValueMatcher rest {args with EOL = head} {status with Mode = LookingForKey; CurrKey = ""}

                    | " " ->
                        keyValueMatcher rest args {status with Mode = LookingForKey; CurrKey = ""}
                    | _ ->
                        keyValueMatcher rest {args with EOL = head} {status with Mode = LookingForKey; CurrKey = ""}

                | Skip ->
                    match tryMatchSkip chars with
                    | Error reason ->
                        Error reason

                    | Ok (num, newRest) ->
                        keyValueMatcher newRest {args with Skip = num} (resetStatus status)

                | Tokens ->
                    match tryParseTokenExpression chars with
                    | Error reason ->
                        Error reason

                    | Ok (tokens, newRest) ->
                        keyValueMatcher newRest {args with Tokens = tokens} (resetStatus status)

                | _ ->
                    printfn "!!!!!!!!!!!!!!!!!!!!!!!!!"
                    printfn "!! NOT YET IMPLEMENTED !!"
                    printfn "!!!!!!!!!!!!!!!!!!!!!!!!!"
                    Error FeatureNotImplemented



    let parseForFArgs (keywords: string) =
        //
        // The only allowed keywords are:
        //
        //   - eol
        //   - skip
        //   - delims
        //   - tokens
        //   - usebackq
        //
        // Keywords are case-insensitive, however they will cause
        // a syntax error if they contain invalid values.  For example,
        // the `skip' keyword sets the number of lines to skip, however
        // it actually accepts any positive, non-zero integer, and may
        // be encoded in either DECIMAL, HEX, or OCTAL.
        //
        // Another thing to note is any additional (unrecognised) parse
        // keywords will cause a syntax error.  Also, when wishing to set
        // the `delims' value to a literal space, it must appear last in
        // the string.
        //
        let status = {
            CurrKey = ""
            CurrValue = ""
            Mode = LookingForKey
        }

        let defaultTokensExpr = {
            Cols = []
            UseWildcard = false
        }

        let defaultArgs = {
            Skip = 0
            UseBackq = false // TODO: is this the correct default value?
            Delims = " "
            EOL = ";" // TODO: is this default correct?
            Tokens = defaultTokensExpr
        }

        let chars = keywords |> List.ofSeq |> List.map (fun x -> x.ToString())
        keyValueMatcher chars defaultArgs status