%x DQS SQS ESC SET SETDQS IF FOR ASSIGN

%{

function token (state, name) {

    let obj = {
        name  : name,
        match : state.match,
        text  : yytext,
    };

    return obj;
}
%}

Any                         [\n\s\w\d\W]
Literal                     [^\^\(\)\[\]\(\)\{\}\s,;=\xff\x0b\x0c\"]+
SetLiteral                  [^=]+
SetValue                    ([^&]{1,2})+
AnyExceptLF                 [^\n]
Delimiter                   [\n\s,;=\xff\x0b\x0c]+
LParen                      "("
RParen                      ")"
DoubleQuote                 [\"]
Escape                      "^"
Equals                      "="
DoubleEquals                "=="

RedirectIn                  "<"
RedirectOut                 ">"
RedirectOutTo               \d\>
RedirectOutAppend           ">>"
RedirectPipe                "|"
RedirectStdoutToStdErr      \&\d

ConditionalAlways           "&"
ConditionalSuccess          "&&"
ConditionalOr               "||"

Set                         [sS][eE][tT]
Call                        [cC][aA][lL]{2}

If                          [iI][fF]                     // IF
Else                        [eE][lL][sS][eE]             // ELSE
Not                         [nN][oO][tT]                 // NOT
Exist                       [eE][xX][iI][sS][tT]         // EXIST
Defined                     [dD][eE][fF][iI][nN][eE][dD] // DEFINED

If_EQU                      [eE][qQ][uU]                 // EQU (equal to)
If_NEQ                      [nN][eE][qQ]                 // NEQ (not equal to)
If_LSS                      [lL][sS]{2}                  // LSS (less than)
If_LEQ                      [lL][eE][qQ]                 // LEQ (less than or equal to)
If_GTR                      [gG][tT][rR]                 // GTR (greater than)
If_GEQ                      [gG][eE][qQ]                 // GEQ (greater than or equal to)

For                         [fF][oO][rR]                 // FOR
In                          [iI][nN]                     // IN
Do                          [dD][oO]                     // DO

%%

<INITIAL>{Escape}             { this.begin("ESC"); return token(this, "ESCAPE"); }
<ESC>{AnyExceptLF}            { this.popState(); return token(this, "ESCAPED_LITERAL"); }

<INITIAL>{LParen}          { return token(this, "LPAREN"); }
<INITIAL>{RParen}          { return token(this, "RPAREN"); }

<INITIAL>{DoubleEquals}           { return token(this, "DOUBLE_EQUALS"); }

<INITIAL>{RedirectStdoutToStdErr} { return token(this, "REDIRECT_STDERR_STDOUT"); }

<INITIAL,SET>{ConditionalSuccess} { this.popState(); return token(this, "COND_SUCCESS"); }
<INITIAL,SET>{ConditionalAlways}  { this.popState(); return token(this, "COND_ALWAYS"); }
<INITIAL,SET>{ConditionalOr}      { this.popState(); return token(this, "COND_OR"); }

// Redirection
<INITIAL>{RedirectIn}         { return token(this, "REDIRECT_IN");  }
<INITIAL>{RedirectOutAppend}  { return token(this, "REDIRECT_OUT_APPEND"); }
<INITIAL>{RedirectOutTo}      { return token(this, "REDIRECT_OUT_TO"); }
<INITIAL>{RedirectOut}        { return token(this, "REDIRECT_OUT"); }
<INITIAL>{RedirectPipe}       { return token(this, "REDIRECT_PIPE"); }

// SET
<INITIAL>{Set}                { this.begin("SET"); return token(this, "SET"); }
<SET>{Equals}                 { this.popState(); this.begin("ASSIGN"); return token(this, "SET_ASSIGN"); }
<ASSIGN>{SetValue}            { this.popState(); return token(this, "SET_VALUE"); }

<INITIAL>{Call}            { return token(this, "CALL"); }

// IF STATEMENT
<INITIAL>{If}                 { return token(this, "IF"); }
<INITIAL>{Else}                    { return token(this, "ELSE"); }
<INITIAL>{Not}                     { return token(this, "NOT"); }
<INITIAL>{Defined}                 { return token(this, "DEFINED"); }
<INITIAL>{Exist}                   { return token(this, "EXIST"); }

<INITIAL>{If_EQU}                  { return token(this, "IF_EQU"); }
<INITIAL>{If_NEQ}                  { return token(this, "IF_NEQ"); }
<INITIAL>{If_LSS}                  { return token(this, "IF_LSS"); }
<INITIAL>{If_LEQ}                  { return token(this, "IF_LEQ"); }
<INITIAL>{If_GTR}                  { return token(this, "IF_GTR"); }
<INITIAL>{If_GEQ}                  { return token(this, "IF_GEQ"); }

// FOR LOOP
<INITIAL>{For}                     { return token(this, "FOR"); }
<INITIAL>{In}                          { return token(this, "IN"); }
<INITIAL>{Do}                          { return token(this, "DO"); }


<INITIAL,SET,FOR>{Delimiter}       { return token(this, "DELIMITER"); }
<SET>{SetLiteral}             { return token(this, "SET_VAR"); }

<INITIAL>{DoubleQuote}     { this.begin("DQS"); return token(this, "STRING_DQUOTE_START"); }
<DQS>[^\"]+                   { return token(this, "STRING_DQUOTE_CHAR"); }
<DQS>{DoubleQuote}            { this.popState(); return token(this, "STRING_DQUOTE_END"); }


<INITIAL>{Literal}         { return token(this, "LITERAL"); }
<*>{Any}                      { return token(this, "UNKNOWN"); }
<*><<EOF>>                    { return token(this, "EOF"); }
