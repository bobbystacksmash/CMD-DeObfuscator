const JisonLex = require("jison-lex"),
      fs       = require("fs");

const grammar = fs.readFileSync(require.resolve("../grammar/cmd.l")).toString(),
      lexer   = new JisonLex(grammar);

function tokenise (cmdstr) {

    lexer.setInput(cmdstr);
    let tokens = [];

    while (true) {
        let token = lexer.lex();
        tokens.push(token);
        if (token.name === "EOF") break;
    }

    return tokens;

}

module.exports = tokenise;
