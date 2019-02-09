type ExpanderState = {
    BuildingVarName: bool
    VarName: string
    LookingForVarExpression: bool
    VarExpression: string
    FindReplace: bool
    Substring: bool
    Vars: Map<string,string>
}

let appendToVarName ctx (chr: char) =
    {ctx with VarName = ctx.VarName + chr.ToString()}

let appendToVarExpression ctx (chr: char) =
    {ctx with VarExpression = ctx.VarExpression + chr.ToString()}

let varExists ctx (varname: string) =
    Map.containsKey (varname.ToUpper()) ctx.Vars

let getVarValueByName ctx (name: string) =
    if varExists ctx name then
        printfn "KEY EXISTS"
        ctx.Vars.[(name.ToUpper())]
    else
        printfn "KEY NOT FOUND"
        name

let gotoLookingForVarExpression ctx =
    {ctx with BuildingVarName = false; LookingForVarExpression = true}

let gotoSubstring ctx =
    printfn "Substring mode"
    {ctx with FindReplace = false; Substring = true}

let gotoFindReplace ctx =
    printfn "FindReplace mode"
    {ctx with FindReplace = true; Substring = false}

let rec percentExpander (cmdstr: char list) (ctx: ExpanderState) =

    printfn "VarChars: %A" ctx.VarName

    match cmdstr with
    | head :: rest ->
        match head with
        | '%' when List.isEmpty rest ->
            // This is the beginning of the var expression.
            percentExpander rest { ctx with BuildingVarName = true }

        | ':' when ctx.BuildingVarName ->
            // There's an edge-case here.  Usually, the ':' signals the
            // beginning of either a substring or find/replace operation,
            // however, there is a case where a variable can be defined with
            // a trailing ':', however these variables cannot be used with
            // either the substring or find/replace operation.
            //
            // Let's test to see if our varname ends with ':'...
            let speculativeVarname = ctx.VarName + ":"
            if varExists ctx speculativeVarname then
                percentExpander rest (appendToVarName (gotoLookingForVarExpression ctx) head)
            else
                percentExpander rest (appendToVarExpression (gotoLookingForVarExpression ctx) head)

        | ':' when ctx.LookingForVarExpression && (ctx.VarName.EndsWith ":") ->
            // This is the edge case we discussed above.  It's not allowed.
            // The whole expression is getting thrown out.
            "%" + ctx.VarName + (String.concat "" <| List.map string rest)

        | '~' when ctx.LookingForVarExpression ->
            percentExpander rest (appendToVarExpression (gotoSubstring ctx) head)

        | _ when ctx.LookingForVarExpression ->
            percentExpander rest (appendToVarExpression (gotoFindReplace ctx) head)

        | '%' when ctx.BuildingVarName ->
            // We never switched in to the command extension state.  Try and resolve the
            // variable name.
            getVarValueByName ctx ctx.VarName

        | '=' when ctx.BuildingVarName ->
            // This is not allowed as the '=' is a prohibited character in env var names:
            // https://docs.microsoft.com/en-gb/windows/desktop/ProcThread/environment-variables
            //
            "%" + ctx.VarName + "=" + (String.concat "" <| List.map string rest)

        | _ ->
            percentExpander rest (appendToVarName ctx head)

    | _ -> "OUTPUT"




let cmd = Seq.toList("%COMSPEC:~%".ToCharArray())
let startState = {
    BuildingVarName = false
    VarName = ""
    LookingForVarExpression = false
    VarExpression = ""
    Substring = false
    FindReplace = false
    Vars = Map.empty.Add("COMSPEC:", "C:\\Windows\\System32\\cmd.exe")
}

let expanded = percentExpander cmd startState
printfn "============================================="
printfn "EXPANDED: [%A]" expanded
printfn "============================================="
