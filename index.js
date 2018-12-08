const JisonLex = require("jison-lex"),
      fs       = require("fs");

const grammar = fs.readFileSync(require.resolve("./comspec.l")).toString(),
      lexer   = new JisonLex(grammar);

function tokenise (cmdstr) {

    lexer.setInput(cmdstr);

    let tokens = [];

    while (true) {
        let token = lexer.lex();

        if (token === "EOF") {
            break;
        }

        tokens.push(token);
    }

    return tokens;
}

function deobfuscate_dos_cmd () {}

module.exports = {
    tokenise:    tokenise,
    deobfuscate: deobfuscate_dos_cmd
};
