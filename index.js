const JisonLex = require("jison-lex"),
      fs       = require("fs");

const grammar = fs.readFileSync(require.resolve("./comspec.l")).toString(),
      lexer   = new JisonLex(grammar);

function tokenise (doscmd) {

    lexer.setInput(doscmd);

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

function expand_variables (doscmd, vars) {

    vars = {
        comspec: "C:\\Windows\\System32\\cmd.exe"
    };

    const VAR_REGEXP = /[^%]*%([A-Z][A-Z0-9_]*)[^%]*%/gi;
    var cmd = doscmd.split(""),
        match;

    while ((match = VAR_REGEXP.exec(doscmd)) !== null) {

        let var_value = vars[match[1].toLowerCase()];

        cmd = [
            cmd.slice(0, match.index).join(""),
            var_value,
            cmd.slice(match.index + var_value.length).join("")
        ].filter(x => x.length).join("");
    }

    return cmd.join("");
}

function parser_lookahead(tokens, index) {

    if (tokens[index++]) return tokens[index++];
}

function deobfuscate_dos_cmd (doscmd) {

    doscmd = expand_variables(doscmd);

    let tokens = tokenise(doscmd),
        outbuf = "";

    tokens.forEach((tok, i) => {

        let lookahead = parser_lookahead(tokens, i);

        if (tok.name === "LITERAL") {
            outbuf += tok.text;
        }
        else if (tok.name === "ESCAPE") {
            outbuf += lookahead.text;
        }
        else {
            console.log("?>", tok.name, tok.text);
        }
    });

    return outbuf;
}


deobfuscate_dos_cmd(`p^o^""w^e^r^s^h^e^l^l`);

module.exports = {
    tokenise:    tokenise,
    deobfuscate: deobfuscate_dos_cmd,
    expand_variables: expand_variables
};
