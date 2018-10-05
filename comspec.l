%x DQS PARAM

%{
// code goes here
%}


Whitespace                       \s
Unknown                          .
DoubleQuote                      \"

Switch                           "/"[A-Za-z0-9]+
SwitchAttrBegin                  ":"

%%

<INITIAL>{DoubleQuote}            { this.begin("DQS"); return "STRING_DQUOTE_BEGIN"; }
<DQS>[^\\]{DoubleQuote}           { this.popState();   return "STRING_DQUOTE_END";   }
<DQS>{Ignore}                     { /* Ignore */ }

// Switches
<INITIAL>{Switch}                 { this.begin("PARAM"); return "SWITCH_BEGIN"; }
<PARAM>{SwitchAttrBegin}          { return "SWITCH_ATTR_BEGIN"; }

<INITIAL>{Whitespace}+            return "WHITESPACE";
<*>{Unknown}                      { /* IGNORE */ }
<*><<EOF>>                        return "EOF";
