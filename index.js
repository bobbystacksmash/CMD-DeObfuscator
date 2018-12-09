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

    return cmd;
}

function deobfuscate_dos_cmd (doscmd) {

    let tokens = tokenise(doscmd);
}

module.exports = {
    tokenise:    tokenise,
    deobfuscate: deobfuscate_dos_cmd,
    expand_variables: expand_variables
};
