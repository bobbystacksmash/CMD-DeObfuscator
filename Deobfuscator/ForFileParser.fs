namespace Deobfuscator.For

open Deobfuscator.DomainTypes
open System.Text.RegularExpressions

type ParseArgsMode =
    | LookingForKey
    | LookingForValue


type ParseArgs = {
    CurrKey: string
    CurrValue: string
    Mode : ParseArgsMode
    Skip: int
    Usebackq : bool
    Delims : string
    EOL : string
    Tokens : string
}


type private FindExtractStatus =
    | ExtractSuccess of string * string
    | NoMatch


module ForFileParser =
    // ReactOS For /F Implementation:
    //   https://doxygen.reactos.org/d0/d06/for_8c_source.html#l00129

    let private (|Delims|Skip|EOL|Tokens|Usebackq|Unrecognised|) (str: string) =
        match str.ToLower() with
        | "delims"  -> Delims
        | "skip"    -> Skip
        | "eol"     -> EOL
        | "tokens"  -> Tokens
        | "usebackq" -> Usebackq
        | _ -> Unrecognised str

    let private (|SkipNumBase|Unknown|) str =
        if Regex.IsMatch("^0x[a-f0-9]+$", str) then SkipNumBase 16
        elif Regex.IsMatch("^0[0-7]$", str) then SkipNumBase 8
        elif Regex.IsMatch("^\d+$", str) then SkipNumBase 10
        else Unknown


    let private (|Number|NaN|) str =
        match str with
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
        match value with
        | NaN -> Error KeywordSkipValueIsNotNumeric
        | Number num -> Ok (num, rest)


    let rec private keyValueMatcher chars args =
        match chars with
        | [] ->
            Ok args (* TODO: figure out when this is an error *)

        | head :: rest ->
            match args.Mode with
            | LookingForKey ->
                match head with
                | "=" -> (* TODO: special case is 'usebackq*)
                    // A space represents the end of a key, so switch modes.
                    keyValueMatcher rest {args with Mode = LookingForValue}

                | _ ->
                    keyValueMatcher rest {args with CurrKey = args.CurrKey + head}

            | LookingForValue ->
                printfn "LOOKING FOR VALUE! -> %A" args.CurrKey
                match args.CurrKey with
                | EOL ->
                    keyValueMatcher rest {args with EOL = head}

                | Skip ->
                    match tryMatchSkip rest with
                    | Error reason ->
                        Error reason

                    | Ok (num, newRest) ->
                        keyValueMatcher newRest {args with Skip = num}

                | _ ->
                    printfn "NOT YET IMPLEMENTED"
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
        let args = {
            CurrKey = ""
            CurrValue = ""
            Mode = LookingForKey
            Skip = 0
            Usebackq = false // TODO: is this the correct default value?
            Delims = ""
            EOL = ";" // TODO: is this default correct?
            Tokens = ""
        }

        let chars = keywords |> List.ofSeq |> List.map (fun x -> x.ToString())
        keyValueMatcher chars args