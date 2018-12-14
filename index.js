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
    }).pop();

    // Substring handling
    // ==================
    //
    // There are a few different ways we can apply substrings.  Assume
    // %foo% = "abcdef".
    //
    //  - %foo:~3%      => def
    //  - %foo:~0,3%    => abc
    //  - %foo:~-3%     => def
    //  - %foo:~1,3%    => bcd
    //
    let substr_re    = /%([a-z][0-9a-z_]*):\s*~\s*([+-]?\d+)(?:,([+-]?\d+))?%/ig,
        replacements = [],
        substr_match;

    while ((substr_match = substr_re.exec(cmd))) {

        let var_name     = substr_match[1].toLowerCase(),
            var_value    = vars[var_name],
            substr_start = substr_match[2],
            substr_end   = substr_match[3];

        if (substr_start !== undefined) {
            substr_start = parseInt(substr_start, 10);
        }

        if (substr_end !== undefined) {
            substr_end = parseInt(substr_end, 10);
        }

        if (var_value === undefined) {
            continue;
        }

        let replace = {
            search: substr_match.input
        };

        let rev = s => s.split("").reverse().join("");

        if ((substr_start !== undefined) && (substr_end === undefined)) {

            if (substr_start == 0) {
                // Special case -- when the substr pattern is
                // something like:
                //
                //   %FOO:~0%
                //
                // Windows expands this to the full variable value.
                replace.replace = var_value;
                replacements.push(replace);
                continue;
            }
            else if (substr_start < 0) {
                // Negative substr values start from the last char and
                // substr forwards.
                let rev_var_value = rev(var_value);
                var_value = rev(rev_var_value.substr(0, (substr_start * -1)));

                replace.replace = var_value;
                replacements.push(replace);
                continue;
            }

            replace.replace = var_value.substring(substr_start, substr_end);
        }
        else if ((substr_start !== undefined) && (substr_end !== undefined)) {

            if (substr_start < 0 && substr_end < 0) {

                console.log();

                substr_start = (substr_start * -1);
                substr_end   = (substr_end   * -1);

                let tmpstart = Math.min(substr_start, substr_end),
                    tmpend   = Math.max(substr_start, substr_end);

                replace.replace = rev(rev(var_value).split("").slice(tmpstart, tmpend).join(""));
            }
            else if (substr_start < 0 && substr_end > 0) {
                let substr_offset = (substr_end + substr_start);
                replace.replace = rev((rev(var_value).substr(substr_offset)));
            }
            else if (substr_end < 0 && substr_start === 0) {
                replace.replace = rev(rev(var_value).substr(substr_end * -1));
            }
            else if (substr_start === 0) {
                replace.replace = var_value.substring(0, substr_end);
            }
            else if (substr_start > 0) {
                replace.replace = var_value.substring(substr_start, substr_end + substr_start);
            }
        }

        replacements.push(replace);
    }

    replacements.forEach(r => cmd = cmd.replace(r.search, r.replace));
    return cmd;
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
        doscmd = expand_variables(doscmd, options.vars);
    }

    let tokens = tokenise(doscmd),
        outbuf = "";

    let skip_token = false;

    tokens.forEach((tok, i) => {

        if (skip_token) {
            skip_token = false;
            return;
        }

        let lookahead = parser_lookahead(tokens, i);

        if (tok.name === "LITERAL") {
            outbuf += tok.text;
            return;
        }
        else if (tok.name === "ESCAPE") {

            if (lookahead.text === " ") {
                // We ignore escaped space sequences.
            }
            else {
                outbuf += lookahead.text;
            }

            return;
        }
        else if (tok.name === "STRING_DQUOTE_BEGIN") {
            if (lookahead.name === "STRING_DQUOTE_END") {
                // We do not copy empty strings to the output buffer.
                skip_token = true;
                return;
            }
        }
        else {
            console.log("?>", tok.name, tok.text);
        }
    });

    return outbuf;
}

module.exports = {
    tokenise:    tokenise,
    deobfuscate: deobfuscate_dos_cmd,
    expand_variables: expand_variables
};
