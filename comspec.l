%x DQS SQS ESC SET SETDQS

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
Set                              [Ss][Ee][Tt]
Assignment                       "="
CallOp                           "&"
Whitespace                       [ \s\t]
Unknown                          .
NewLine                          [\r\n]
DoubleQuote                      [\"]
SingleQuote                      "'"
Any                              .
NextCommand                      "&"
Comma                            ","
SemiColon                        ";"

%%

<INITIAL,SET>{CallOp}           { return token(this, "CALL"); }

<INITIAL>{Set}              { this.begin("SET"); return token(this, "SET"); }
<SET,SETDQS>{Assignment}    { return token(this, "SET_ASSIGNMENT"); }
<SET>{DoubleQuote}          { this.begin("SETDQS"); return token(this, "SET_DQUOTE_BEGIN"); }
<SETDQS>{DoubleQuote}       { this.popState(); return token(this, "SET_DQUOTE_END"); }
<SETDQS>[^\"]               { return token(this, "SET_DQUOTE_CHAR"); }

<INITIAL>{Comma}            { return token(this, "COMMA"); }
<INITIAL>{SemiColon}        { return token(this, "SEMICOLON"); }

<INITIAL,SQS>{Escape}       { this.begin("ESC"); return token(this, "ESCAPE"); }

<INITIAL>{DoubleQuote}      { this.begin("DQS"); return token(this, "STRING_DQUOTE_BEGIN"); }
<DQS>{DoubleQuote}          { this.popState(); return token(this, "STRING_DQUOTE_END");  }
<DQS>[^\"]                  { return token(this, "STRING_DQUOTE_CHAR"); }

<INITIAL>{SingleQuote}      { this.begin("SQS"); return token(this, "STRING_SQUOTE_BEGIN"); }
<SQS>{SingleQuote}          { this.popState(); return token(this, "STRING_SQUOTE_END");  }
<SQS>[^\']                  { return token(this, "STRING_SQUOTE_CHAR"); }


<ESC>{Any}                  { this.popState(); return token(this, "ESCAPED_LITERAL"); }



<*>{Unknown}                { return token(this, "LITERAL"); }
<*><<EOF>>                  { return "EOF"; }
