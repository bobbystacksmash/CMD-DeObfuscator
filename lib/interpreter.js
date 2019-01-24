const varexpand = require("./var-expander").expand,
      tokenise  = require("./tokeniser");

const DEFAULTS = {
    strip_escapes: true,
    strip_empty_strings: true,
    unify_strings_and_literals: true
};

function tokens2str (tokens) {
    return tokens.map(t => t.text).join("");
}

function token_convert(token, new_name) {
    token = Object.assign({}, token);

    let old_name = token.name;
    token.name  = new_name;

    if (old_name === "STRING_DQUOTE") {
        token.text = token.text.replace(/^"|\"$/g, "");
    }

    return token;
}

function is(token, names) {

    if (names instanceof RegExp) {
        return names.test(token.name);
    }
    else if (typeof names === "string") {
        return names === token.name;
    }
    else if (Array.isArray(names)) {
        return names.indexOf(token.name) >= 0;
    }

    return false;
}


function FILTER_strip_escapes (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    tokens = tokens.filter(t => t.name !== "ESCAPE");
    for (let i = 0; i < tokens.length; i++) {
        if (tokens[i].name === "ESCAPED_LITERAL") {
            tokens[i].name = "LITERAL";
        }
    }

    return tokens;
}


function FILTER_strip_empty_strings (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    return tokens.filter(t => {
        if (t.name === "STRING_DQUOTE" && t.text === `""`) {
            return false;
        }
        return true;
    });
}


function FILTER_unify_strings_and_literals (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    let is_str_or_lit = (tok) => is(tok, ["STRING_DQUOTE", "LITERAL"]),
        is_str        = (tok) => is(tok, "STRING_DQUOTE"),
        is_lit        = (tok) => is(tok, "LITERAL");

    let run    = [],
        result = [];

    for (let i = 0; i < tokens.length; i++) {

        let token = tokens[i];

        if (is_str_or_lit(token)) {
            run.push(token);
        }
        else {
            result.push(run);
            run = [];
            result.push(token);
        }
    }
    if (run.length > 0) result.push(run);

    let output = [];
    for (let i = 0; i < result.length; i++) {
        let elem = result[i];

        if (!Array.isArray(elem)) {
            output.push(elem);
        }
        else if (elem.length === 1) {
            output.push(elem.pop());
        }
        else {

            let str = elem.map(e => e.text).join("");

            if (!elem.some(e => is_str(e))) {
                output.push({
                    name: "LITERAL",
                    text: str
                });
            }
            else {
                output.push({
                    name: "STRING_DQUOTE",
                    text: `"${str.replace(/["]/g, "")}"`
                });
            }
        }
    }

    return output;
}


function interpret (cmdstr, options) {

    options = options || {};
    options = Object.assign({}, DEFAULTS, options);

    // Phase 1: expand environment variables.
    let expanded = varexpand(cmdstr),
        tokens   = tokenise(expanded);

    if (options.strip_escapes) {
        tokens = FILTER_strip_escapes(tokens);
    }

    if (options.strip_empty_strings) {
        tokens = FILTER_strip_empty_strings(tokens);
    }

    if (options.unify_strings_and_literals) {
        tokens = FILTER_unify_strings_and_literals(tokens);
    }

    return [tokens2str(tokens)];
}

module.exports = interpret;
