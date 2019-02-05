open System.Runtime.Remoting.MetadataServices
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
// To unlock various parts of the command line such that we can see the commands
// in the clear, we need to implement parts of the above commands.
//
let (|MetaCharacter|LiteralCharacter|) chr =
    match chr with
    | '('
    | ')'
    | '%'
    | '!'
    | '^'
    | '"'
    | '<'
    | '>'
    | '&'
    | '|' -> MetaCharacter
    | _   -> LiteralCharacter

// Tracks the parser's current state.  When 'IgnoreMeta' is TRUE, we will treat
// all MetaCharacters as LiteralCharacters, with the exception of:
//
//   1. Identification of a variable substring operation.
//   2. The end of the line.
//   3. Seeing another '"'.
//
type ParserState = { IgnoreMeta: bool }