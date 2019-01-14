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


Delimiter                   [\s,;=]+

%%

<INITIAL>{Delimiter}        { return token(this, "DELIMITER"); }

<*>{Unknown}                { return token(this, "LITERAL"); }
<*><<EOF>>                  { return token(this, "EOF"); }
