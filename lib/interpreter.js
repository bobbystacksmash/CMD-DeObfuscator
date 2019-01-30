const varexpand = require("./var-expander").expand,
      tokenise  = require("./tokeniser"),
      getcmd    = require("./commands"),
      path      = require("path").win32;

const DEFAULTS = {
    strip_escapes: true,
    merge_contiguous_literals: true,
    merge_contiguous_strings: true,
    strip_empty_strings: true,
    unify_strings_and_literals: true,
    only_commands: false
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

function last (arr) {
    return arr[arr.length - 1];
}

function has (obj, key) {
    return obj.hasOwnProperty(key);
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


function FILTER_merge_contiguous_literals (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    let newtokens = [];

    for (let i = 0; i < tokens.length; i++) {
        let token     = tokens[i],
            lookahead = tokens[i + 1];

        if (is(token, "LITERAL") && lookahead && is(lookahead, "LITERAL")) {
            token.text += lookahead.text;
            i++;
        }
        newtokens.push(token);
    }

    return newtokens;
}


function FILTER_merge_contiguous_strings (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    let newtokens = [];

    for (let i = 0; i < tokens.length; i++) {
        let token     = tokens[i],
            lookahead = tokens[i + 1];

        if (is(token, "STRING_DQUOTE") && lookahead && is(lookahead, "STRING_DQUOTE")) {

            let tmp = token.text.replace(/"$/, "");
            token.text = tmp + lookahead.text.replace(/^"/, "");
            i++;
        }
        newtokens.push(token);
    }

    return newtokens;
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

function cmd_head_rest (tokens) {

    let head = [],
        rest = [];

    let headindex = tokens.findIndex(t => is(t, ["COND_SUCCESS", "COND_ALWAYS", "COND_OR"]));

    if (headindex < 0) {
        return {
            head: tokens,
            rest: []
        };
    }

    return {
        head: tokens.slice(0, headindex),
        rest: tokens.slice(headindex + 1)
    };
}

function HANDLE_set (identcmd, context) {

    if (!context.length) {
        context.unshift({
            vars: {
                nextframe: {},
                thisframe: {}
            },
            commands: []
        });
    }

    // TODO: handle SET arguments.
    let cmd_rest = cmd_head_rest(identcmd.rest);

    context[0].commands.push({
        command: { name: "set", line: cmd_rest.head.map(x => x.text).join("") },
        options: {}
    });

    let _var = null,
        _val = null;

    for (let i = 0; i < cmd_rest.head.length; i++) {
        let token = cmd_rest.head[i];
        if (is(token, "SET_VAR")) {
            _var = token.text;
        }
        else if (is(token, "SET_VALUE")) {
            context[0].vars.nextframe[_var.toLowerCase()] = token.text;
        }
    }

    if (cmd_rest.rest.length) {
        interpret(
            cmd_rest.rest.map(x => x.text).join("").replace(/^\s*"|"\s*$/g, ""),
            null,
            context
        );
    }
}


function HANDLE_call (identcmd, context) {

    let cmd_rest = cmd_head_rest(identcmd.rest);

    // CALL creates a new context.
    context.unshift({
        vars: {
            nextframe: context[0].vars.thisframe,
            thisframe: context[0].vars.nextframe
        },
        commands: []
    });

    // We push the CALL command so its listed in the output commands
    // array.
    context[0].commands.push({
        command: { name: identcmd.command, line: cmd_rest.head.map(x => x.text).join("") },
        options: {}
    });

    if (cmd_rest.head.length) {
        interpret(
            cmd_rest.head.map(x => x.text).join("").replace(/^\s*"|"\s*$/g, ""),
            { vars: context[0].vars.thisframe },
            context
        );
    }
}


function HANDLE_default (identcmd, context) {

    let cmd_rest = cmd_head_rest(identcmd.rest);

    if (!context.length) {
        // TODO: create new context
        context.unshift({
            vars: {
                thisframe: {},
                nextframe: {}
            },
            commands: []
        });
    }

    context[0].commands.push({
        command: { name: identcmd.command, line: cmd_rest.head.map(x => x.text).join("") },
        options: {}
    });

    if (cmd_rest.rest.length) {
        interpret(
            cmd_rest.rest.map(x => x.text).join("").replace(/^\s*"|"\s*$/g, ""),
            null,
            context
        );
    }
}

function add_new_context () {

}


function parse_cmd_options (tokens) {
    // Flags are valid whether or not they're wrapped in DQS.
    let switchre = /\/([A-Z])([:][^\s]+)?(?:$|\s)/ig;
    const switch_lookup = {
        "c": "run_then_terminate",
        "C": "run_then_terminate",
        "v": "delayed_expansion",
        "V": "delayed_expansion",
        "e": "cmd_extensions",
        "E": "cmd_extensions",
        "f": "path_autocomplete",
        "F": "path_autocomplete"
    };

    let looking_for_switches = true,
        restindex            = -1,
        switches             = {};

    tokens.forEach((t, i) => {

        if (!looking_for_switches) return;

        if (is(t, ["STRING_DQUOTE", "LITERAL"]) && /\/[a-z]/i.test(t.text)) {
            let match;
            while ((match = switchre.exec(t.text))) {

                let wholematch       = match[0],
                    _switch          = match[1],
                    _value           = "",
                    match_end_offset = match[0].length + match.index;

                if (match[2] !== undefined) {
                    _value = match[2].replace(/^:/, "");
                }

                if (/^[efv]$/i.test(_switch)) {
                    _switch = switch_lookup[_switch];
                    switch (_value.toLowerCase()) {
                    case "off":
                        _value = false;
                        break;
                    default:
                        _value = true;
                    }
                }
                else if (has(switch_lookup, _switch)) {
                    _switch = switch_lookup[_switch];
                    _value  = true;
                }

                switches[_switch] = _value;
                restindex = i + 1;
            }
        }
        else {
            looking_for_switches = false;
            restindex = i;
        }
    });

    return { switches: switches, rest: tokens.slice(restindex) };
}


function HANDLE_cmd (identcmd, context) {

    let switchobj = parse_cmd_options(identcmd.rest),
        nextcmd   = switchobj.rest;

    // CMD always creates a new context.
    context.unshift({
        vars: {
            nextframe: {},
            thisframe: (context.length && has(context[0], "vars")) ? context[0].vars.nextframe : {}
        },
        options: {},
        commands: []
    });

    // We always copy the options from a CMD in to the frame's global
    // options.
    context[0].options = switchobj.switches;

    context[0].commands.push({
        command: {
            name: "cmd",
            line: nextcmd.map(x => x.text).join("").replace(/^\s*|\s*$/g, "")
        },
        options: switchobj.switches
    });

    let nestedcmd = switchobj.rest.map(x => x.text).join("").replace(/^\s*"|"\s*$/g, "");

    if (nestedcmd) {
        interpret(nestedcmd, null, context);
    }
}

function try_identify_command (tokens) {
    tokens = Array.prototype.slice.call(tokens);

    const condfilters = ["COND_ALWAYS", "COND_SUCCESS", "COND_OR"];

    // First, we strip off any leading delimiter tokens.
    let flag      = true,
        tmptokens = [];
    for (let i = 0; i < tokens.length; i++) {
        if (flag && is(tokens[i], ["LPAREN", "DELIMITER", "RPAREN"].concat(condfilters))) {
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
    let delimindex = tmptokens.findIndex(x => is(x, ["DELIMITER"].concat(condfilters))),
        cmdtokens  = [],
        resttokens = [];
    if (delimindex > 0) {
        cmdtokens = tmptokens.slice(0, delimindex);
    }
    else {
        cmdtokens = tmptokens;
    }

    if (delimindex >= 0) {
        resttokens = tmptokens.slice(delimindex + 1);
    }
    else {
        resttokens = [];
    }

    // Remove all closing parens.
    cmdtokens = cmdtokens.filter(t => !is(t, "RPAREN"));

    let original = cmdtokens.map(t => t.text).join(""),
        clean    = original.replace(/["]/g, "").replace(/[)]$/g),
        output   = { command: null, rest: resttokens };



    if (/^[a-z]:/i.test(clean) || /^[.]{1,2}[\\/]/.test(clean)
        || /^[\\/]/.test(clean) || /[\\/][a-z0-9]+(?:\.[a-z0-9]+)?$/i.test(clean)) {
        output.command  = path.basename(clean).toLowerCase().replace(/^\s*|\s*$/g, "").replace(/\.exe$/i, "");
    }
    else {
        output.command = clean.split(/ /)[0].toLowerCase().replace(/\.exe$/i, "");
    }

    return output;
}

function interpret (cmdstr, options, context) {

    options = options || {};
    options = Object.assign({}, DEFAULTS, options);
    context = context || [];

    // Phase 1: expand environment variables.
    //
    // We take any env vars from the current context and merge it with
    // existing vars.
    if (context && context.length && last(context) && has(last(context), "variables")) {
        options.vars = Object.assign({}, options.vars, last(context).variables);
    }

    // Expand environment variables.
    const framevars  = (context.length && has(context[0], "vars")) ? context[0].vars.thisframe : {};
    let expanded = varexpand(cmdstr, { vars: framevars });
    if (context.length && has(context[0], "options") && has(context[0].options, "delayed_expansion")) {
        const vars = Object.assign({}, framevars, context[0].vars.nextframe);
        expanded = varexpand(expanded, { vars: vars, sigil: "!" });
    }

    let tokens     = tokenise(expanded),
        cmd_blocks = get_cached_cmd_blocks(tokens);

    if (options.strip_escapes) {
        tokens = FILTER_strip_escapes(tokens);
    }

    if (options.strip_empty_strings) {
        tokens = FILTER_strip_empty_strings(tokens);
    }

    if (options.merge_contiguous_literals) {
        tokens = FILTER_merge_contiguous_literals(tokens);
    }

    if (options.merge_contiguous_strings) {
        tokens = FILTER_merge_contiguous_strings(tokens);
    }

    let identcmd = try_identify_command(tokens);

    switch (identcmd.command) {
    case "cmd":
        HANDLE_cmd(identcmd, context);
        break;
    case "set":
        HANDLE_set(identcmd, context);
        break;
    case "call":
        HANDLE_call(identcmd, context);
        break;
    default:
        HANDLE_default(identcmd, context);
    }

    return context;
}

module.exports = {
    interpreter: interpret,
    get_cached_cmd_blocks: get_cached_cmd_blocks,
    try_identify_command: try_identify_command
};
