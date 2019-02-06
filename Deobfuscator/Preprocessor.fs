module Deobfuscator.Preprocessor
    //
    // A METHOD FOR THINKING ABOUT WINDOWS CMD LINE STRINGS
    // ====================================================
    //
    // There is no one way to parse command lines on Windows.  Command line
    // parsing is different depending upon which binary is being executed,
    // and CMD.EXE is no different.  Ultimately, it's a binary which accepts
    // a command line, which it parses.
    //
    // What makes CMD.EXE interesting is the pre-processing it performs on its
    // input tokens.  Rather than trying to "chunk up" the input in to useful
    // units, it will first perform transformations to the input string, such
    // as expanding environment variables.  Once performed, CMD.EXE may then
    // pass the modified command string to another program, which in turn uses
    // its own parser to make sense of the input tokens.
    //
    // The game, therefore, is to implement a set of command line parsers which
    // behave in a way that matches those implemented by the major commands used
    // within a CMD.EXE session, including:
    //
    //   - CMD.EXE
    //   - SET
    //   - IF
    //   - FOR
    //   - 'generic handler'
    //
    let (|EscapeCharacter|MetaCharacter|QuoteCharacter|LiteralCharacter|) chr =
        match chr with
        | '('
        | ')'
        | '%'
        | '!'
        | '<'
        | '>'
        | '&'
        | '|' -> MetaCharacter
        | '^' -> EscapeCharacter
        | '"' -> QuoteCharacter
        | _   -> LiteralCharacter

    // Tracks the parser's current state.  When 'IgnoreMeta' is TRUE, we will treat
    // all MetaCharacters as LiteralCharacters, with the exception of:
    //
    //   1. Identification of a variable substring operation.
    //   2. The end of the line.
    //   3. Seeing another '"'.
    //
    type ParserState = {
        IgnoreMeta: bool
        Escape: bool
        DelayedExpansion: bool
        VarExpression: bool
        EnvironmentVars: Map<string,string>
    }

    let DefaultParserState () =
        let defaultCtx = {
            IgnoreMeta = false
            Escape = false
            VarExpression = false
            DelayedExpansion = false
            EnvironmentVars = Map.empty
                .Add("COMSPEC", "C:\\Windows\\System32\\cmd.exe")
                .Add("PATHEXT", ".COM;.EXE;.BAT;.JS")
        }
        defaultCtx

    type TokenMeta = char
    type TokenLiteral = char
    type Token =
        | Meta of TokenMeta
        | Literal of TokenLiteral

    let AppendMeta lst chr = (List.append lst [Meta(chr)])
    let AppendLiteral lst chr = (List.append lst [Literal(chr)])

    let rec Tokenise (context: ParserState) (chars: char list) (col: Token list) =
        let ignoreMetaChars = context.IgnoreMeta
        let escape = context.Escape
        match Seq.toList(chars) with
        | head :: rest ->
            match head with
            | EscapeCharacter when ignoreMetaChars ->
                Tokenise context rest (AppendLiteral col head)
            | EscapeCharacter when escape ->
                Tokenise { context with Escape = false } rest (AppendLiteral col head)
            | EscapeCharacter ->
                Tokenise { context with Escape = true } rest col
            | QuoteCharacter when ignoreMetaChars ->
                Tokenise { context with IgnoreMeta = false; } rest (AppendMeta col head)
            | QuoteCharacter ->
                Tokenise { context with IgnoreMeta = true; } rest (AppendMeta col head)
            | MetaCharacter when (head = '%' || (head = '!' && context.DelayedExpansion)) ->
                Tokenise context rest (AppendMeta col head)
            | MetaCharacter when (ignoreMetaChars || escape) ->
                Tokenise { context with Escape = false } rest (AppendLiteral col head)
            | MetaCharacter ->
                Tokenise context rest (AppendMeta col head)
            | _ ->
                Tokenise context rest (AppendLiteral col head)
        | [] -> col

    let Preprocess (ctx: ParserState) (cmdstr: string) =
        Tokenise ctx (Seq.toList(cmdstr.ToCharArray())) (List.empty)