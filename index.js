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

    const default_vars = {
        appdata: "C:\\Users\\whoami\\AppData\\Roaming",
        comspec: "C:\\Windows\\System32\\cmd.exe"
    };
    vars = Object.assign(default_vars, vars);

    let cmd = Object.keys(vars).map(varname => {
        return doscmd.replace(new RegExp(`%${varname}%`, "gi"), vars[varname]);
    });

    return cmd.pop();
}

function parser_lookahead(tokens, index) {

    if (tokens[index++]) return tokens[index++];
}

function deobfuscate_dos_cmd (doscmd, options) {

    const default_opts = {
        expand_vars: true
    };
    options = Object.assign(default_opts, options || {});

    if (default_opts.expand_vars) {
        doscmd = expand_variables(doscmd);
    }

    let tokens = tokenise(doscmd),
        outbuf = "";

    tokens.forEach((tok, i) => {

        let lookahead = parser_lookahead(tokens, i);

        if (tok.name === "LITERAL") {
            outbuf += tok.text;
            return;
        }
        else if (tok.name === "ESCAPE") {
            outbuf += lookahead.text;
            return;
        }
        else {
            console.log("?>", tok.name, tok.text);
        }
    });

    console.log(outbuf);

    return outbuf;
}

module.exports = {
    tokenise:    tokenise,
    deobfuscate: deobfuscate_dos_cmd,
    expand_variables: expand_variables
};
