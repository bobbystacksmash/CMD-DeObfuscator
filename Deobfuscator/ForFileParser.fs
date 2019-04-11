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
        if Regex.IsMatch(str, @"^0x[a-f0-9]+$")
        then
            SkipNumBase 16
        elif Regex.IsMatch(str, "^0[0-7]$") 
        then 
            SkipNumBase 8
        elif Regex.IsMatch(str, "^\d+$") 
        then 
            SkipNumBase 10
        else 
            Unknown


    let private (|Number|NaN|) str =
        match str with
        | Unknown -> NaN
        | SkipNumBase numBase ->
            printfn "Skip num base matched..!!!!!"
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
        printfn "tryMatchSkip (value, rest) = %A, %A" value rest
        match value with
        | NaN ->
            let msg = sprintf "Expected FOR /F 'skip=' value (%s) to be numeric." value
            Error (KeywordSkipValueIsNotNumeric msg)
        | Number num -> Ok (num, rest)


    let rec private keyValueMatcher chars (args: ForLoopParsingArgs) status =
        match chars with
        | [] ->
            Ok args (* TODO: figure out when this is an error *)

        | head :: rest ->
            match status.Mode with
            | LookingForKey ->
                match head with
                | "=" -> (* TODO: special case is 'usebackq*)
                    // A space represents the end of a key, so switch modes.
                    keyValueMatcher rest args {status with Mode = LookingForValue}

                | _ ->
                    keyValueMatcher rest args {status with CurrKey = status.CurrKey + head}

            | LookingForValue ->
                printfn "LOOKING FOR VALUE! -> %A" status.CurrKey
                match status.CurrKey with
                | EOL ->
                    printfn "GET EOL VALUE!!!!"
                    keyValueMatcher rest args status

                | Skip ->
                    match tryMatchSkip chars with
                    | Error reason ->
                        Error reason

                    | Ok (num, newRest) ->
                        // TODO: Fix this part.
                        printfn "HERE %A / %A" num newRest
                        keyValueMatcher newRest {args with Skip = num} status // TODO: what about status?

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
        let status = {
            CurrKey = ""
            CurrValue = ""
            Mode = LookingForKey
        }
        
        let defaultArgs = {
            Skip = 0
            UseBackq = false // TODO: is this the correct default value?
            Delims = " "
            EOL = ";" // TODO: is this default correct?
            Tokens = "" // TODO: is a string the correct data type?
        }

        let chars = keywords |> List.ofSeq |> List.map (fun x -> x.ToString())
        keyValueMatcher chars defaultArgs status