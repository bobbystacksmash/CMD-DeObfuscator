namespace Deobfuscator

open Deobfuscator.DomainTypes
open System.Text.RegularExpressions


// TODO: Add the parsed arguments block to each
//       loop type.
type ForLoopType =      // |======|
    | ForFiles          // |  /F  |
    | ForFileContents   // |  /F  |
    | ForCommandResults // |  /F  |
    | ForFolders        // |  /D  |
    | ForNumberList     // |  /L  |
    | ForFilesAtPath    // |  /R  |


type ForLoopExpression = {
    Header: Token list
    Set: Token list
}

type IfExpression = {
    Test: Token list
}


type Expression =
    | ForLoopExpr of ForLoopExpression * Expression


type Keyword =
    | Lit of string
    | For
    | In
    | Do
    | Rem
    | Echo


type ForLoopHeader = {
    Type: ForLoopType
    Var: string
    // TODO: figure out what we do about the loop args...
}


type Statement =
    | OperatorStmt of Operator
    | ForLoopStmt of ForLoopType



module StatementMatcher =

    (* UTILITIES *)
    let private stringifyTokens lst =
        let rec stringify lst accum =
            match lst with
            | [] -> accum |> List.rev
            | x::xs ->
                match x with
                | Literal str
                | Delimiter str -> stringify xs (str :: accum)

        stringify lst []


    let private stripDelimiters lst =
        lst |> List.filter (function | Delimiter _ -> false | _ -> true)

    let rec ltrimDelimiters lst =
        match lst with
        | [] -> lst
        | head :: rest ->
            match head with
            | Delimiter _ -> ltrimDelimiters rest
            | _ -> lst


    let rtrimDelimiters lst =
        lst |> List.rev |> ltrimDelimiters |> List.rev


    let trimDelims lst =
        lst |> rtrimDelimiters |> ltrimDelimiters



    let private listEndsWith (lst: Token list) ending =
        if lst.Length = 0
        then false
        else
            let last = lst |> List.rev |> List.head
            last.ToString().ToUpper() = ending


    (*
       =================================
       FOR loop identification & parsing
       =================================
    *)

    (*let private parseForLoopF hdrRest =
        printfn "ForLoop/F header -> %A" hdrRest
        Ok LooksGood


    let private parseForLoopR hdrRest =
        printfn "ForLoop/R header -> %A" hdrRest
        Ok LooksGood


    let private parseForLoopWithFlag loopFlag hdrRest =
        // Looking at examples for the different kinds of flagged FOR loop,
        // loop header complexity only really concerns loop types:
        //
        //   - /F (ForFiles)
        //   - /R (ForFilesAtPath)
        //
        // Loop types `/D' and `/L' do not have arguments.
        //
        match loopFlag with
        | ForFiles -> parseForLoopF hdrRest
        | ForFilesAtPath -> parseForLoopR hdrRest
        | ForFolders
        | ForNumberList ->
          Ok LooksGood
        | _ ->
            Error FeatureNotImplemented


    let (|ForLoopFlag|_|) (str: string) =
        match str.ToUpper() with
        | "/D" -> Some (ForLoopFlag ForFolders)
        | "/F" -> Some (ForLoopFlag ForFiles)
        | "/L" -> Some (ForLoopFlag ForNumberList)
        | "/R" -> Some (ForLoopFlag ForFilesAtPath)
        | _ -> None


    let private isValidForLoopVar str =
        Regex.IsMatch (str, @"^%[A-Z]", RegexOptions.IgnoreCase)


    let (|UnvalidatedForHeader|InvalidLoop|) (hdr: string list) =
        let x = hdr |> List.rev |> List.map (fun x -> x.ToUpper())
        printfn "X = %A" x

        match (hdr |> List.rev |> List.map (fun x -> x.ToUpper())) with
        | [] ->
            InvalidLoop HeaderListIsEmpty

        | [_; loopVar]
        | [_; loopVar; _]
        | [_; loopVar; _; _] when not (isValidForLoopVar loopVar) ->
            InvalidLoop LoopVariableIsNotValid

        | ["IN"; loopVar] ->
            // FOR %A IN...
            UnvalidatedForHeader { Flag = ""; Var = loopVar; Args = ""}

        | ["IN"; loopVar; flag] ->
            // FOR /L %A IN...
            UnvalidatedForHeader { Flag = flag; Var = loopVar; Args = "" }

        | ["IN"; loopVar; args; flag] ->
            // FOR /F "args args" IN...
            UnvalidatedForHeader { Flag = flag; Var = loopVar; Args = args }

        | _ when hdr.Length > 4 ->
            InvalidLoop HeaderListIsTooLong

        | _ ->
            InvalidLoop FlagOrLoopVarExpected



    let private (|ValidForHeader|InvalidForHeader|) (hdr: InterimForLoopHeader) =

        printfn "Attempting to validate header: %A" hdr

        // TODO: work starts here...
        //
        // Fields which need validating are:
        //
        //   - hdr.Flag
        //   - hdr.Var
        //   - hdr.Args
        //
        // While we /can/ pattern match on a record, it's a bit nicer to
        // use a list for this phase...
        //
        let forHeader = { Var = hdr.Var; Type = ForFiles}

        match [hdr.Flag.ToUpper(); hdr.Args; hdr.Var] with
        | [_; _; var] when not (isValidForLoopVar var) ->
            // Loop vars must look something like "%A"
            let msg = sprintf "Invalid loop variable '%s' detected." var
            InvalidForHeader (LoopVariableIsNotValid msg)

        | [""; ""; var] ->
            // Matches a 'basic' FOR loop, e.g.: FOR %A IN...
            ValidForHeader forHeader

        | ["/L"; ""; var] ->
            // This type of loop does not use args.
            ValidForHeader forHeader

        | ["/D"; ""; var] ->
            // Nor does this one.
            ValidForHeader forHeader

        | ["/R"; args; var] ->
            // Args may or may not be empty.
            ValidForHeader forHeader

        | ["/F"; args; var] ->
            // This one needs A LOT of attention.
            ValidForHeader forHeader

        | _ ->
            InvalidForHeader FeatureNotImplemented


    let private tryParseForLoop forHdr astRest =
        // The `forHdr' should look something like the examples below.
        // The '|' (pipe) in the `INPUT' examples show the header boundary.
        //
        //   INPUT > |FOR %A IN| (1 2 3) DO echo %A
        //   AST   > [Literal "%A"; Literal "IN"]
        //
        //   INPUT > |FOR /f "usebackq delims==" %i in| ('set') do @echo %i
        //   AST   > [Literal "/F"; Literal ""usebackq delims==""; Literal "%i"; Literal "in"]
        //
        match stripDelimiters forHdr |> stringifyTokens with
        | InvalidLoop reason -> Error reason
        | UnvalidatedForHeader hdr ->
            match hdr with
            | ValidForHeader valid ->
                printfn "Got a valid header! %A" valid
                Error FeatureNotImplemented
    *)

    //
    // Keyword Identification
    // ----------------------
    // Handles only high-level identification of keywords.
    //
    (*let private (|ForKeyword|IfKeyword|RemKeyword|Other|Invalid|) (command: Token list) =
        let trimmed = trimDelims command
        match trimmed with
        | [] -> Invalid
        | head :: rest ->
            match head with
            | Literal instruction ->
                let trimmedRest = ltrimDelimiters rest
                match instruction.ToUpper() with
                | "FOR" ->
                    ForKeyword (stripDelimiters rest)

                | "IF"  ->
                    IfKeyword trimmedRest

                | "REM" ->
                    RemKeyword trimmedRest

                | _ -> Other trimmedRest
            | Delimiter _ -> Invalid
    *)






    (* Walking the AST *)
    let rec private walk ast =
        match ast with
        | [] -> true // Obviously returning bools isn't useful here.
        | head :: rest ->
            match head with
            | Op oper ->
                printfn "OPERATOR -> %A" oper
                walk rest

            | Cmd cmd ->
                printfn "COMMAND -> %A" cmd
                walk rest


    let rec tryConvertParseTreeToAst ptree =
        let result = walk ptree
        Ok ptree
