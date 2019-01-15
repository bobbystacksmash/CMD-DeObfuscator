const JisonLex = require("jison-lex"),
      fs       = require("fs");

const grammar = fs.readFileSync(require.resolve("../grammar/cmd.l")).toString(),
      lexer   = new JisonLex(grammar);

function tokenise (cmdstr) {

    lexer.setInput(cmdstr);
    let tokens = [];

    let strbuf  = [],
        strflag = false;

    while (true) {

        let token = lexer.lex();

        if (token.name === "EOF") {

            if (strflag) {
                // We encountered the EOF while building a string.
                // Shove all strbuf tokens in to the major tokens
                // array.
                tokens = tokens.concat(strbuf);
            }

            break;
        }

        if (token.name === "STRING_DQUOTE_START") {
            strbuf = [token];
            strflag = true;
            continue;
        }
        else if (token.name === "STRING_DQUOTE_CHAR") {
            strbuf.push(token);
            continue;
        }
        else if (token.name === "STRING_DQUOTE_END") {
            strbuf.push(token);
            // We've now got a bunch of individual tokens stored
            // within the strbuf array.  We want to collapse them all
            // in to a single token.
            tokens.push({
                name: "STRING_DQUOTE",
                text: strbuf.map(s => s.text).join("")
            });

            strflag = false;

            continue;
        }

        tokens.push(token);
    }

    return tokens;

}

module.exports = tokenise;
