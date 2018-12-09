%x DQS ESC VAR PARAM

%{
function token (state, name) {

    let obj = {
        name  : name,
        match : state.match,
        line  : yylineno,
        text  : yytext,
        len   : yyleng,
        loc   : yylloc
    };

    return obj;
}
%}

Variable                         "%[A-Z]+%"
Escape                           "^"
Whitespace                       [ \s\t]
Unknown                          .
NewLine                          [\r\n]
DoubleQuote                      [\"]
SingleQuote                      "'"
Any                              .

%%

<INITIAL>{DoubleQuote}      { this.begin("DQS"); return token(this, "STRING_DQUOTE_BEGIN"); }
<DQS>{DoubleQuote}          { this.popState(); return token(this, "STRING_DQUOTE_END");  }
<DQS>[^\"]                  { return token(this, "STRING_DQUOTE_CHAR"); }

<INITIAL>{Escape}           { this.begin("ESC"); return token(this, "ESCAPE"); }
<ESC>{Any}                  { this.popState(); return token(this, "ESCAPED_LITERAL"); }



<*>{Unknown}                { return token(this, "LITERAL"); }
<*><<EOF>>                  { return "EOF"; }
