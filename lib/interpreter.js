const varexpand = require("./var-expander").expand,
      tokenise  = require("./tokeniser");

const DEFAULTS = {
    strip_escapes: true,
    strip_empty_strings: true,
};

function tokens2str (tokens) {
    return tokens.map(t => t.text).join("");
}

function FILTER_strip_escapes (tokens) {
    tokens = Array.prototype.slice.call(tokens);
    return tokens.filter(t => t.name !== "ESCAPE");
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

    return [tokens2str(tokens)];
}

module.exports = interpret;
