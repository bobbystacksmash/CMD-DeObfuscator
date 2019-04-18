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
        | _          -> Unrecognised str


    let private (|Hex|Dec|Oct|Unknown|) str =
        if Regex.IsMatch(str, @"^0x[0-9a-f]+$", RegexOptions.IgnoreCase)
        then
            Hex(str)
        elif Regex.IsMatch(str, @"^0[0-7]+$")
        then
            Oct(str)
        elif Regex.IsMatch(str, @"^\d+$")
        then
            Dec(str)
        else
            Unknown


    let private (|PositiveInt|Zero|NegativeInt|NaN|) str =

        let toNumBase (numstr: string) (numbase: int) =
            try
                let num = (System.Convert.ToInt32(numstr, numbase))
                match num with
                | _ when num < 0 -> NegativeInt num
                | _ when num = 0 -> Zero num
                | _ when num > 0 -> PositiveInt num
                | _ -> NaN
            with
                _ -> NaN

        match str with
        | Hex num -> toNumBase str 16
        | Dec num -> toNumBase str 10
        | Oct num -> toNumBase str  8
        | Unknown -> NaN


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
        let (value, rest) = getValue chars " "
        match value with
        | NaN ->
            let msg = sprintf "Expected FOR /F 'skip=' value (%s) to be numeric." value
            Error (KeywordSkipValueIsNotNumeric msg)

        | Zero num
        | NegativeInt num ->
            Error (KeywordSkipCannotBeZero "Using 'skip=0' is not allowed.")

        | PositiveInt num ->
            Ok (num, rest)


    let private (|TokenRange|WildcardRange|Col|WildcardCol|WC|NotValid|) (expr: string) =

        let endsWithWildcard = Regex.IsMatch(expr, "[*]$")
        let strippedWildcard = Regex.Replace(expr, "[*]$", "")

        if strippedWildcard.Contains "-"
        then
            let parts = strippedWildcard.Split [|'-'|] |> List.ofSeq
            match parts with
            | [PositiveInt rangeFrom; PositiveInt rangeTo] when endsWithWildcard ->
                WildcardRange [rangeFrom .. rangeTo]

            | [PositiveInt rangeFrom; PositiveInt rangeTo] ->
                TokenRange [rangeFrom .. rangeTo]

            | _ -> NotValid
        else
            match strippedWildcard with
            | PositiveInt num when endsWithWildcard ->
                WildcardCol num

            | PositiveInt num ->
                Col num

            | _ when expr = "*" ->
                WC

            | _ ->
                NotValid


    let private expandTokenRanges expr =
        match expr with
        | TokenRange range
        | WildcardRange range ->
            range |> List.map (fun x -> Column(x))

        | Col col ->
            [Column col]

        | WildcardCol col ->
            [Column col; Wildcard]

        | WC ->
            [Wildcard]

        | NotValid ->
            [Invalid expr]


    let private (|ValidTokenExpr|_|) (exprs: TokenColumn list) =

        let invalidExprs =
            exprs
            |> List.filter (fun exp ->
                match exp with
                | Invalid _ -> true
                | _ -> false)


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

        let zeroOrNegatives =
            columns
            |> List.filter (fun x -> x <= 0)

        if invalidExprs.Length > 0 then
            None
        elif zeroOrNegatives.Length > 0 then
            None
        elif numWildcards.Length > 1 then
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

        let (value, rest) = getValue chars " "
        //
        // Handle the curious case where the following is INVALID:
        //
        //   `tokens= eol=;`
        //
        // ...however, the following is VALID:
        //
        //   `tokens= ` (note: trailing space)
        //
        if value = "" && rest.Length > 0 then
            let errmsg = sprintf "Expected value on right-hand side of '%s'" (chars |> List.fold (+) "")
            Error (ExpectedParseKeywordValue errmsg)
        else
            let columns =
                value.Split [|','|]
                |> List.ofSeq
                |> List.collect expandTokenRanges

            match columns with
            | ValidTokenExpr tokensExpr ->
                Ok (tokensExpr, rest)
            | _ ->
                Error (KeywordTokensIsInvalid "Invalid 'tokens=' value provided.")


    let private tryParseDelims (chars: string list) =
        let (value, rest) = getValue chars " "
        let delims = value |> List.ofSeq
        Ok (delims, rest)


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
                    // TODO: handle the case where the value is a SPC
                    match tryParseTokenExpression chars with
                    | Error reason ->
                        Error reason

                    | Ok (tokens, newRest) ->
                        keyValueMatcher newRest {args with Tokens = tokens} (resetStatus status)

                | Delims ->
                    match tryParseDelims chars with
                    | Error reason ->
                        Error reason
                    | Ok (delims, newRest) ->
                        keyValueMatcher newRest {args with Delims = delims} (resetStatus status)


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
            Delims = [' '; '\t']
            EOL = ";" // TODO: is this default correct?
            Tokens = defaultTokensExpr
        }

        let chars = keywords |> List.ofSeq |> List.map (fun x -> x.ToString())
        keyValueMatcher chars defaultArgs status