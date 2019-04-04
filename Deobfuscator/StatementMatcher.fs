namespace Deobfuscator

open Deobfuscator.DomainTypes
open System.Text.RegularExpressions

                        // | FLAG |
type ForLoopType =      // |======|
    | ForFiles          // |  /F  |
    | ForFileContents   // |  /F  |
    | ForCommandResults // |  /F  |
    | ForFolders        // |  /D  |
    | ForNumberList     // |  /L  |
    | ForFilesAtPath    // |  /R  |


type InvalidForLoopHeaderReasons =
    | FlagOrLoopVarExpected of string option
    | UnknownFlag of string
    | HeaderListIsEmpty of string option
    | MissingInKeyword of string option
    | MissingVariableIdentifier of string option
    | VariableIdentifierNotValid of string option
    | CannotFindForInKeyword of string option


type ForLoopHeader = {
    LoopType: ForLoopType
    Var: string
}


type Statement =
    | OperatorStmt of Operator
    | ForLoopStmt of ForLoopType



// TODO
//   At some point we'll split the StatementMatcher out in to its
//   own module.
module StatementMatcher =

    (* UTILITIES *)
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
        FOR loop identification
    *)

    let private (|ForInstruction|_|) (instr: Token) =
        match instr with
        | Literal strcmd ->
            match strcmd.ToUpper() with
            | "FOR" -> Some ForInstruction
            | _ -> None
        | _ -> None


    // Typically, the `args' seen here are the following part
    // of a FOR loop:
    //
    //   FOR %A in (1 2 3) DO echo %A
    //      ^^^^^^^
    //
    (*let (|ForLoopFlag|ForLoopNoFlag|Invalid|) hdr =
        match (stripDelimiters hdr) with
        | [] ->
            ForLoopNoFlag ForFiles

        | [Literal maybeFlag] ->
            match maybeFlag.ToUpper() with
            | "/D" -> ForLoopFlag ForFolders
            | "/F" -> ForLoopFlag ForFileContents
            | "/L" -> ForLoopFlag ForNumberList
            | "/R" -> ForLoopFlag ForFilesAtPath

    let (|ForLoopVar|Missing|) hdr =
            match (stripDelimiters hdr |> List.rev) with
            | [] -> Missing MissingVariableIdentifier
            | head :: rest ->
                match head with
                | Literal maybeVar ->
                    let m = Regex.Match(maybeVar, @"^%[A-Z]$", RegexOptions.IgnoreCase)
                    if m.Success
                    then ForLoopVar (m.Groups.[0].Value.ToString, (rest |> List.rev))
                    else Missing VariableIdentifierNotValid
                | _ ->
                    Missing MissingVariableIdentifier


    let (|ForLoopIn|Missing|) hdr =
        match (stripDelimiters hdr |> List.rev) with
        | [] -> Missing
        | head :: rest ->
            match head with
            | Literal str when str.ToUpper() = "IN" -> ForLoopIn (rest |> List.rev)
            | _ -> Missing
    *)

    (*let (|ForLoopHeader|InvalidForLoopHeader|) hdr =
        //
        // Assuming what we've been passed via `hdr' is from a valid FOR loop
        // we should expect to see one of the lists:
        //
        //   1. ["%A" ; "IN"]
        //   2. ["/F" ; "%A" ; "IN"]
        //
        // The first thing we'll check is that the array ends with the `IN'
        // keyword...
        match hdr with
        | Missing -> InvalidForLoopHeader CannotFindForInKeyword
        | ForLoopIn hdrTokens ->
            match hdrTokens with
            | Missing reason -> InvalidForLoopHeader reason
            | ForLoopVar (loopVar, remainingHdr) ->
                match remainingHdr with
                |
*)

    let tryParseForSet hdr =
        Ok hdr


    let tryParseForDirs hdr =
        Ok hdr


    let tryParseForList hdr =
        Ok hdr


    let tryParseForRooted hdr =
        Ok hdr


    let tryParseForLoopArgs (flag: string) (hdrRest: Token list) (astRest: Ast list) =
        Ok hdrRest
        (*match flag.ToUpper() with
        | "/F" -> tryParseForSet     hdrRest
        | "/D" -> tryParseForDirs    hdrRest
        | "/L" -> tryParseForList    hdrRest
        | "/R" -> tryParseForRooted  hdrRest
        | _ -> Error (UnknownFlag (sprintf "FOR loop flag '%A' is not recognised." flag))*)


    let (|ForLoopFlag|_|) str =
        if Regex.IsMatch(str, "^/[DFLR]$", RegexOptions.IgnoreCase)
        then Some ForLoopFlag
        else None

    let (|ForLoopVar|_|) str =
        if Regex.IsMatch (str, @"^%[A-Z]", RegexOptions.IgnoreCase)
        then Some ForLoopVar
        else None


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
        // The `forHdr' list contains much of the FOR-loop parsing
        // complexity.
        //
        let hdr = stripDelimiters forHdr
        match hdr with
        | [] -> Error HeaderListIsEmpty
        | head :: rest ->
            match head with
            | Literal firstToken ->
                match firstToken with
                // TODO: figure out how to fix this part.
                | ForLoopFlag -> tryParseForLoopArgs firstToken rest astRest
                | ForLoopVar  -> tryParseForLoopHeader firstToken rest astRest
                | _ ->
                    Error FlagOrLoopVarExpected
            | _ ->
                Error FlagOrLoopVarExpected


    let (|MaybeForLoop|MaybeConditional|MaybeRemark|Other|Invalid|) (command: Token list) =
        let trimmed = trimDelims command
        match trimmed with
        | [] -> Invalid
        | head :: rest ->
            match head with
            | Literal instruction ->
                let trimmedRest = ltrimDelimiters rest
                match instruction.ToUpper() with
                (* FOR %A IN...   *)
                | "FOR" ->
                    MaybeForLoop (stripDelimiters rest)

                (* IF "a"=="b"... *)
                | "IF"  ->
                    MaybeConditional trimmedRest

                (* REM ...        *)
                | "REM" ->
                    MaybeRemark trimmedRest

                | _ -> Other trimmedRest
            | Delimiter _ -> Invalid


    let rec walk ast =
        match ast with
        | [] -> true
        | head :: rest ->
            match head with
            // We have four `Statement' types:
            //
            //   1. OperatorStmt
            //   2. ForLoopStmt
            //   3. IfStmt
            //   4. RemStmt
            //
            // While walking the AST, we identify the different elements
            // and determine what Statement type they should be converted
            // to.  Statements: IF, FOR, and REM receive special handling
            // and cannot be generated later in the interpretation process.
            // For example, using Delayed Expansion to expand a var which
            // contains a valid IF statement will always cause a syntax
            // error.
            //
            // For more details on this behaviour, see:
            //
            //   > https://www.dostips.com/forum/viewtopic.php?t=5416
            //
            | Op _ ->
                printfn "OPERATOR"
                walk rest

            | Cmd cmd ->
                match cmd with
                | MaybeForLoop loop ->
                    tryParseForLoop loop rest
                    walk rest




    let rec enhanceAst ast =
        let result = walk ast
        Ok ast


    let liftAst maybeAst =
        match maybeAst with
        | Ok ast -> enhanceAst ast
        //| Error reason ->
        //    Error reason
