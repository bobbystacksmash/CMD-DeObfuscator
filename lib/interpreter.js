const varexpand = require("./var-expander").expand,
      tokenise  = require("./tokeniser"),
      path      = require("path").win32;

const DEFAULTS = {
    strip_escapes: true,
    strip_empty_strings: true,
    unify_strings_and_literals: true,
    only_commands: false,
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


/*function FILTER_unify_strings_and_literals (tokens) {
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

            if (!str.includes(" ")) {
                // The string doesn't contain any spaces, so we won't
                // add double quotes anywhere and convert this token
                // to a LITERAL.
                output.push({
                    name: "LITERAL",
                    text: str.replace(/["]/g, "")
                });
                continue;
            }

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
}*/

function get_cached_cmd_blocks (tokens) {

    let split_block = (t) => (is(t, ["COND_SUCCESS", "COND_ALWAYS", "COND_OR"])),
        block       = [],
        blocks      = [];

    tokens.forEach(t => {

        if (split_block(t) && block.length) {
            blocks.push(block);
            block = [];
        }
        else {
            block.push(t);
        }
    });
    if (block.length) blocks.push(block);

    return blocks;
}


function run (tokens, collector) {
    collector.push(tokens2str(tokens));
}


function try_identify_command (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    // First, we strip off any leading delimiter tokens.
    let flag      = true,
        tmptokens = [];
    for (let i = 0; i < tokens.length; i++) {
        if (flag && is(tokens[i], ["LPAREN", "DELIMITER"])) {
            continue;
        }
        else {
            flag = false;
        }
        tmptokens.push(tokens[i]);
    }

    // Then, we take the set of tokens up to the NEXT delimiter.  This
    // forms the basis of what we'll be looking at to identify what
    // the command is.
    let delimindex = tmptokens.findIndex(x => is(x, "DELIMITER")),
        cmdtokens  = [],
        resttokens = [];
    if (delimindex > 0) {
        cmdtokens = tmptokens.slice(0, delimindex);
    }
    resttokens = tmptokens.slice(delimindex);

    let original = tmptokens.map(t => t.text).join(""),
        clean    = original.replace(/["]/g, "").replace(/[\)]+$/g);

    if (/^[a-z]:/i.test(clean) || /^[.]{1,2}[\\/]/.test(clean)
        || /^[\\/]/.test(clean) || /[\\/][a-z0-9]+(?:\.[a-z0-9]+)?$/i.test(clean)) {
        return {
            cmd: path.basename(clean).replace(/\.(?:exe|com)$/, "").toLowerCase(),
            rest: resttokens
        };
    }
    else {
        return {
            cmd: clean.split(/ /)[0].replace(/\.[a-z]+$/, ""),
            rest: resttokens
        };
    }
}


function interpret (cmdstr, options) {

    options = options || {};
    options = Object.assign({}, DEFAULTS, options);

    // Phase 1: expand environment variables.
    let expanded   = varexpand(cmdstr),
        tokens     = tokenise(expanded),
        cmd_blocks = get_cached_cmd_blocks(tokens);

    if (options.strip_escapes) {
        tokens = FILTER_strip_escapes(tokens);
    }

    if (options.strip_empty_strings) {
        tokens = FILTER_strip_empty_strings(tokens);
    }

    if (options.unify_strings_and_literals) {
        //tokens = FILTER_unify_strings_and_literals(tokens);
    }

    let collector = [];
    run(tokens, collector);

    return collector;
}

module.exports = {
    interpreter: interpret,
    get_cached_cmd_blocks: get_cached_cmd_blocks,
    try_identify_command: try_identify_command
};
