%x DQS SQS ESC SET SETDQS

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
AnyExceptLF                 [^\n]
Delimiter                   [\n\s,;=\xff\x0b\x0c]+
LParen                      "("
RParen                      ")"
DoubleQuote                 [\"]
Escape                      "^"

RedirectIn                  "<"
RedirectOut                 ">"
RedirectOutTo               \d\>
RedirectOutAppend           ">>"
RedirectPipe                "|"
RedirectStdoutToStdErr      \&\d

ConditionalAlways           "&"
ConditionalSuccess          "&&"
ConditionalOr               "||"

%%

<INITIAL>{Escape}             { this.begin("ESC"); return token(this, "ESCAPE"); }
<ESC>{AnyExceptLF}            { this.popState(); return token(this, "ESCAPED_LITERAL"); }

<INITIAL>{LParen}             { return token(this, "LPAREN"); }
<INITIAL>{RParen}             { return token(this, "RPAREN"); }

<INITIAL>{RedirectStdoutToStdErr} { return token(this, "REDIRECT_STDERR_STDOUT"); }

<INITIAL>{ConditionalSuccess} { return token(this, "COND_SUCCESS"); }
<INITIAL>{ConditionalAlways}  { return token(this, "COND_ALWAYS"); }
<INITIAL>{ConditionalOr}      { return token(this, "COND_OR"); }

// Redirection
<INITIAL>{RedirectIn}         { return token(this, "REDIRECT_IN");  }
<INITIAL>{RedirectOutAppend}  { return token(this, "REDIRECT_OUT_APPEND"); }
<INITIAL>{RedirectOutTo}      { return token(this, "REDIRECT_OUT_TO"); }
<INITIAL>{RedirectOut}        { return token(this, "REDIRECT_OUT"); }
<INITIAL>{RedirectPipe}       { return token(this, "REDIRECT_PIPE"); }



<INITIAL>{Delimiter}+         { return token(this, "DELIMITER"); }

<INITIAL>{DoubleQuote}        { this.begin("DQS"); return token(this, "STRING_DQUOTE_START"); }
<DQS>[^\"]+                   { return token(this, "STRING_DQUOTE_CHAR"); }
<DQS>{DoubleQuote}            { this.popState(); return token(this, "STRING_DQUOTE_END"); }


<*>{Any}                      { return token(this, "LITERAL"); }
<*><<EOF>>                    { return token(this, "EOF"); }
