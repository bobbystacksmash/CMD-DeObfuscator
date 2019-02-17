namespace Deobfuscator

open System.Text.RegularExpressions

//
// TOKENISER
//
type private Matcher =
    | MatchingSpecialChars
    | IgnoringSpecialChars

type private TokeniserState = {
    Escape: bool
    Mode: Matcher
}

type Token =
    | LeftParen     of string // (
    | RightParen    of string // )
    | Literal       of string
    | Delimiter     of string
    | CondSuccess   of string // &&
    | CondAlways    of string // &
    | CondOr        of string // ||
    | LeftRedirect  of string // <
    | RightRedirect of string // >
    | Pipe          of string // |
    | Quote         of string // "
    static member (+) (a, b) =
        match a, b with
        | Literal a0, Literal a1 -> Literal (a0 + a1)
        | Delimiter d0, Delimiter d1 -> Delimiter (d0 + d1)
        | CondAlways c0, CondAlways c1 -> CondSuccess (c0 + c1)
        | Pipe p0, Pipe p1 -> CondOr (p0 + p1)
        | _ -> failwith "invalid arguments to perform token join"

    static member CanConcat =
        function
            | Literal _
            | Delimiter _
            | Pipe _
            | CondAlways _ -> true
            | _ -> false


module Tokeniser =

    let sameType a b = a.GetType() = b.GetType()

    let private (|ESC|QUOTE|LPAREN|RPAREN|LREDIRECT|RREDIRECT|OTHER|) str =
        match str with
        | "^"  -> ESC   str
        | "\"" -> QUOTE str
        | "("  -> LPAREN str
        | ")"  -> RPAREN str
        | "<"  -> LREDIRECT str
        | ">"  -> RREDIRECT str
        | _    -> OTHER str

    let private (|AMPERSAND|PIPE|OTHER|) str =
        match str with
        | "&" -> AMPERSAND str
        | "|" -> PIPE str
        | _   -> OTHER str

    let private (|DELIM|OTHER|) chr =
        match chr with
        | ","
        | "="
        | " "
        | ";" -> DELIM chr
        | _   -> OTHER chr

    let appendToken (lst: Token list) tok =

        let safeRest (lst: Token list) =
            if lst.Length = 0 then []
            else lst |> List.tail

        if lst.Length > 0 then
            let prevTok = lst.Head
            if Token.CanConcat(tok) && sameType tok prevTok then
                (prevTok + tok) :: (lst |> safeRest)
            else
                tok :: lst
        else
            tok :: lst


    let rec private tokeniseList (cmdstr: string list) (ctx: TokeniserState) acc =

        match cmdstr with
        | chr::rest ->
            match chr with
            | _ when ctx.Escape ->
                tokeniseList rest {ctx with Escape=false} (appendToken acc (Literal chr))

            | ESC _ when ctx.Mode = MatchingSpecialChars ->
                tokeniseList rest {ctx with Escape=true} acc

            | QUOTE _ when ctx.Mode = MatchingSpecialChars ->
                tokeniseList rest {ctx with Escape=false; Mode=IgnoringSpecialChars} (appendToken acc (Quote "\""))

            | QUOTE _ ->
                tokeniseList rest {ctx with Mode=MatchingSpecialChars} (appendToken acc (Quote "\""))

            | _ when ctx.Mode = IgnoringSpecialChars ->
                tokeniseList rest ctx (appendToken acc (Literal(chr)))

            | LPAREN lp ->
                tokeniseList rest ctx (appendToken acc (LeftParen lp))

            | RPAREN rp ->
                tokeniseList rest ctx (appendToken acc (RightParen rp))

            | LREDIRECT lrd ->
                tokeniseList rest ctx (appendToken acc (LeftRedirect lrd))

            | RREDIRECT rrd ->
                tokeniseList rest ctx (appendToken acc (RightRedirect rrd))

            | DELIM delim ->
                tokeniseList rest ctx (appendToken acc (Delimiter delim))

            | AMPERSAND amp ->
                tokeniseList rest ctx (appendToken acc (CondAlways amp))

            | PIPE pipe ->
                tokeniseList rest ctx (appendToken acc (Pipe pipe))

            | _ ->
                tokeniseList rest ctx (appendToken acc (Literal chr))

        | _ -> acc |> List.rev


    let tokenise cmdstr =
        let state = { Escape = false; Mode = MatchingSpecialChars }
        cmdstr.ToString()
        |> Seq.toList
        |> List.map (fun ch -> ch.ToString())
        |> (fun x -> tokeniseList x state [])


    let buildAST (tokens: Token list) =

        let rec walk (lst: Token list) col acc =
            printfn "----------------------------"
            printfn "COL -> %A" col
            printfn "ACC -> %A" acc
            printfn "----------------------------"

            match lst with
            | head :: rest ->
                match head with
                | LeftParen  _ ->
                    walk rest [] (col :: acc)

                | RightParen _ ->
                    walk rest [] (col :: acc)

                | _ ->
                    walk rest (head :: col) acc
            | _ -> acc

        walk tokens [] []
