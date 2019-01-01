const JisonLex = require("jison-lex"),
      path     = require("path").win32,
      fs       = require("fs");

const grammar = fs.readFileSync(require.resolve("./comspec.l")).toString(),
      lexer   = new JisonLex(grammar);

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;                                    ;;
// ;; TYPE DEFINITIONS FOR DOCUMENTATION ;;
// ;;                                    ;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//
// Token
// =====
/**
 * @typedef {Object} Token
 * @property {string} name  - Name of the token (LITERAL, SET, ESCAPE, ...).
 * @property {string} match - Match object for the matched token.
 * @property {string} line  - Line number upon which the token was found.
 * @property {string} text  - Similar to 'match'.
 * @property {number} len   - Length (in chars) of the matched token.
 * @property {Object} loc   - Location of match (first/last line, first/last col).
 */


/**
 * Parses a given command string in to individual commands, before
 * applying expansion and de-obfuscation filters to the each command.
 *
 * @param {string} cmdstr - The original command string to be
 * de-obfuscated.
 *
*/
function parse_cmdstr (cmdstr, options) {

    options = options || {};

    let vars   = {},
        output = [];

    split_command(cmdstr).forEach(curr_cmd => {

        let cmd = run_command(curr_cmd);

        vars = Object.assign(vars, cmd.vars);

        let expanded_cmd = expand_environment_variables(cmd.command.clean, vars);

        output.push(expanded_cmd);

        //let deobfuscated = deobfuscate_dos_cmd(strip_escape_chars(cmd));
        //vars = Object.assign(deobfuscated.vars, vars);
        //console.log(expand_variables(deobfuscated.command, vars));
    });

    return output;
}

/**
 * Given a command string, attempts to remove all non-quoted
 * contiguous whitespace LITERALS, leaving a single space between each word boundary.
 *
 * @param {Token|Array} tokens - An array of tokens.
 * @returns {Token|Array}
 */
function FILTER_strip_excessive_whitespace (tokens) {

    for (let i = 0; i < tokens.length; i++) {

        let token      = tokens[i],
            lookahead  = tokens[i + 1];

        if (token.name === "LITERAL" && token.text === " ") {
            if (i === 0) {
                tokens.splice(0,1);
                i = -1;
            }
            else if (i === (tokens.length - 1) && token.text === " ") {
                tokens.splice(i, 1);
                i = -1;
            }
            else if (lookahead && lookahead.name === "LITERAL" && lookahead.text === " ") {
                tokens.splice(i, 1);
                i = -1;
            }

        }
    }

    return tokens;
}


/**
 * Given a command string, attempts to slurp all LITERAL,
 * non-whitespace tokens surrounding a string inside that string.  For
 * example:
 *
 *   c"alc".exe  --[slurped]--> "calc.exe"
 *
 * @param {Token|Array} tokens - An array of tokens.
 * @returns {Token|Array}
 */
function FILTER_slurp_literals_into_strings (tokens) {

    for (let i = 0; i < tokens.length; i++) {

        let token      = tokens[i],
            lookahead  = tokens[i + 1],
            lookbehind = tokens[i - 1];

        if (token.name === "STRING_DQUOTE_BEGIN") {
            if (lookbehind && lookbehind.name === "LITERAL" && lookbehind.text !== " ") {
                tokens[i - 1] = token;
                tokens[i]     = lookbehind;
                tokens[i].name = "STRING_DQUOTE_CHAR";
            }
        }
        else if (token.name === "STRING_DQUOTE_END") {
            if (lookahead && lookahead.name === "LITERAL" && lookahead.text !== " ") {
                tokens[i + 1] = token;
                tokens[i]     = lookahead;
                tokens[i].name = "STRING_DQUOTE_CHAR";
            }
        }
    }

    // We need to clean-up the tokens.  Consider the following input:
    //
    //   h"t"t"p"
    //
    // The way the algorithm works, we'll end up with our tokens being
    // ordered:
    //
    //   "htt""p"
    //
    for (i = 0; i < tokens.length; i++) {

        let token       = tokens[i],
            lookahead_1 = tokens[i + 1],
            lookahead_2 = tokens[i + 2];

        if (token.name === "STRING_DQUOTE_END") {
            if (lookahead_1 && lookahead_1.name === "STRING_DQUOTE_BEGIN") {
                tokens.splice(i, 2);
            }
        }
    }

    return tokens;
}

/**
 * Given an array of Tokens, attempts to remove all empty strings ("")
 * from the list, returning a new list of tokens with empty string
 * tokens removed.
 *
 * @param {Token|Array} tokens - An array of tokens.
 * @returns {Token|Array}
 */
function FILTER_strip_empty_strings (tokens) {

    let out_tokens = [],
        skip_token = false;

    for (let i = 0; i < tokens.length; i++) {

        let token      = tokens[i],
            lookahead  = tokens[i + 1];

        if (skip_token) {
            out_tokens.pop();
            skip_token = false;
            continue;
        }

        out_tokens.push(token);

        switch (token.name) {
        case "STRING_DQUOTE_BEGIN":
            if (lookahead && lookahead.name === "STRING_DQUOTE_END") {
                skip_token = true;
            }
            break;
        }
    }

    return out_tokens;
}

/**
 * Given a command string, attempts to partially parse the command,
 * returning an object which can be used to present the command in an
 * easy-to-understand way.
 *
 * @param {string} cmdstr - The command to run/parse.
 * @returns {Object}
 */
function run_command (cmdstr) {

    let clean_cmdstr =
        strip_escape_chars(cmdstr)
        .replace(/^\s+|\s+$/, "");

    let tokens = tokenise(clean_cmdstr),
        flags  = {
            in_set_cmd              : false,
            capturing_env_var_name  : false,
            capturing_env_var_value : false
        };

    let env_vars      = {},
        env_var_name  = "",
        env_var_value = "";

    let command_name  = "UNKNOWN";

    // The `outbuf` var holds a cleaned-up version of the command with
    // all obfuscation removed.
    let outbuf = [];

    // When TRUE, the parser skips the next token.  Used in cases
    // where we want to ignore "".
    let skip = false;

    for (let i = 0; i < tokens.length; i++) {

        if (skip) {
            outbuf.pop();
            skip = false;
            continue;
        }

        let token     = tokens[i],
            lookahead = tokens[i + 1];

        outbuf.push(token.text);

        switch (token.name) {

        case "LITERAL":

            if (flags.in_set_cmd) {
                if (flags.capturing_env_var_name) {
                    env_var_name += token.text;
                }
                else if (flags.capturing_env_var_value) {
                    env_var_value += token.text;
                }
            }
            else {

            }
            break;

        case "SET":
            flags.capturing_env_var_name = true;
            flags.in_set_cmd             = true;
            command_name                 = "SET";
            break;

        case "SET_ASSIGNMENT":
            flags.capturing_env_var_name  = false;
            flags.capturing_env_var_value = true;
            break;

        case "SET_DQUOTE_CHAR":

            if (flags.capturing_env_var_name) {
                env_var_name += token.text;
            }
            else if (flags.capturing_env_var_value) {
                env_var_value += token.text;
            }

            break;

        case "SET_DQUOTE_BEGIN":
        case "SET_DQUOTE_END":
            // TODO: may need to add another flag here...
            break;

        case "STRING_DQUOTE_BEGIN":
            if (lookahead.name === "STRING_DQUOTE_END") {
                skip = true;
            }
            break;

        default:
            //console.log("UNKNOWN TOK>", token.name, token.text);
        }
    }

    if (env_var_name.length && env_var_value.length) {
        env_vars[env_var_name] = env_var_value;
    }

    return {
        command: {
            name       : command_name,
            no_escapes : clean_cmdstr,
            clean      : outbuf.join(""),
            original   : cmdstr
        },
        vars: env_vars
    };
}

/**
 * Given a command string, attempts to split the string, returning an
 * array of individual command strings.
 *
 * @param {string} command - a CMD.EXE command.
 * @returns {string|Array} Each command is an element in the array.
 */
function split_command (command_str) {

    let tokens   = tokenise(command_str),
        index    = 0,
        commands = [""];

    tokens.forEach(tok => {

        if (tok.name === "CALL" || tok.name === "COND_CALL") {
            index++;
            commands[index] = "";
        }
        else {
            commands[index] += tok.text;
        }
    });

    return commands
        .map(cmd => cmd.replace(/^\s*|\s*$/g, "")) // Remove leading and trailing whitespace
        .filter(cmd => ! /^\s*$/.test(cmd));
}

/**
 * Given a command string, attempts to split the string in to an array
 * of Token objects.
 *
 * @param {string} cmdstr - The command string to split in to tokens.
 * @param {string} [options] - Set .filter T|F to enable/disable filtering.
 * @returns {Token|Array} Token objects, one-per-token.
 */
function tokenise (cmdstr, options) {

    options = options || {};
    options = Object.assign({}, { filter: true }, options);

    if (options.filter) {
        cmdstr = cmdstr.replace(/^\s+|\s+$/g, "");
    }

    lexer.setInput(cmdstr);

    let tokens = [];

    while (true) {

        let token = lexer.lex();

        if (token === "EOF") {
            break;
        }

        tokens.push(token);
    }

    if (options.filter) {
        tokens = FILTER_strip_empty_strings(tokens);
        tokens = FILTER_slurp_literals_into_strings(tokens);
        tokens = FILTER_strip_excessive_whitespace(tokens);
    }

    return tokens;
}

/**
 * Attempts to perform a find/replace with variable expansion against
 * a given DOS command with values read from an optional variable
 * key/value object.  BATCH implements some syntactic-sugar to support
 * finding and replacing characters within an environment variable:
 *
 * @example
 * // Replace all 'a' chars with 'b' in var 'foo':
 * "%foo:a=b%"
 *
 * @param {string} cmdstr - DOS command we wish to deobfuscate.
 * @param {Object} [vars] - An object mapping var names to values.
 *
 * @returns {string} An expanded form of `cmdstr` with all variable
 * find/replace operations performed.
 */
function substr_replace (cmdstr, vars) {

    let find_replace_re = /%([a-z][0-9a-z_]*):([^\s]+)=([^\s]+)?%/ig,
        got_match;

    while ((got_match = find_replace_re.exec(cmdstr))) {

        let wholematch = got_match[0],
            findstr    = got_match[2],
            replstr    = got_match[3] || "",
            varname    = got_match[1].toLowerCase(),
            varvalue   = vars[varname];

        if (vars.hasOwnProperty(varname) === false) {
            continue;
        }

        let replaced_varvalue = varvalue.split(findstr).join(replstr);

        cmdstr = cmdstr.split(wholematch).join(replaced_varvalue);
    }

    return cmdstr;
}

/**
 * Given a command string an an object mapping varname => varvalue,
 * attempts to apply the range of text manipulations supported by the
 * BATCH language.  The following features are supported:
 *
 *   - expansion  :: %foo% expands to the valueOf(%foo%).
 *   - substrings :: %foo:~5%, %foo:0,3%, %foo:~-3%
 *   -
 *
 */
function expand_environment_variables (cmdstr, vars) {

     const default_vars = {
        appdata: "C:\\Users\\whoami\\AppData\\Roaming",
        comspec: "C:\\Windows\\System32\\cmd.exe"
    };
    vars = Object.assign(default_vars, vars);

    // Expand Variables
    // ================
    //
    // Take all instances of '%foo%' and replace with the value found
    // within the 'vars' dict.
    //
    let cmd = Object.keys(vars).map(varname => {
        // TODO: I don't think we can use a new RegExp for the
        // replacement because envvar variable names can contain
        // punctuation chars which will conflict with the RegExp
        // engine's metacharacters.
        return cmdstr.replace(new RegExp(`%${varname}%`, "gi"), vars[varname]);
    }).pop();

    // Apply Find/Replace
    // ==================
    //
    // Searches the variable for all instances of STR, replacing with
    // REP, for example:
    //
    //   %foo:STR=REP%
    //   %foo:cat=dog%
    //
    cmd = substr_replace(cmd, vars);

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

function strip_escape_chars (cmdstr, options) {

    let outcmd     = "",
        tokens     = tokenise(cmdstr),
        ignore     = /^(?:ESCAPE)$/,
        skip_token = false;

    tokenise(cmdstr).forEach((tok, i) => {

        if (skip_token) {
            skip_token = false;
            return;
        }

        let next_tok = tokens[i + 1];

        if (ignore.test(tok.name)) {
            return;
        }
        else if (tok.name === "SET_DQUOTE_CHAR" && tok.text === "^") {
            return;
        }
        else if (tok.name === "SET_DQUOTE_BEGIN" && next_tok.name === "SET_DQUOTE_END") {
            skip_token = true;
            return;
        }
        else if (tok.name === "SET_SQUOTE_BEGIN" && next_tok.name === "SET_SQUOTE_END") {
            skip_token = true;
            return;
        }

        outcmd += tok.text;
    });

    return outcmd;
}

/*function deobfuscate_dos_cmd (cmdstr, options) {

    const default_opts = {
        expand_vars: true
    };
    options = Object.assign(default_opts, options || {});

    if (default_opts.expand_vars) {
        cmdstr = expand_environment_variables(cmdstr, options.vars);
    }

    let tokens = tokenise(cmdstr),
        outbuf = [],
        varmap = {},
        varbuf = "",
        valbuf = "";

    // Shove an end marker in so we know we're at the end of the command.
    tokens.push({
        name: "END",
        text: null
    });

    let dqs_skip_token    = false,
        sqs_skip_token    = false,
        in_env_var_name   = false,
        in_env_var_value  = false,
        set_startswith    = null,
        multi_space_chars = true;

    tokens.forEach((tok, i) => {

        if (dqs_skip_token) {
            dqs_skip_token = false;
            return;
        }
        else if (sqs_skip_token) {
            sqs_skip_token = false;
            return;
        }

        let lookahead = tokens[i + 1];

        outbuf.push(tok.text);

        if (tok.name === "LITERAL") {

            if (tok.text !== " " && multi_space_chars) {
                multi_space_chars = false;
            }

            if (in_env_var_name) {
                varbuf += tok.text;
            }
            else if (in_env_var_value) {
                valbuf += tok.text;
            }
            else if (tok.text === " ") {

                if (multi_space_chars) {
                    return;
                }
                else {
                    multi_space_chars = true;
                }
            }

            return;
        }
        else if (tok.name === "ESCAPE") {
            console.log("PRE", outbuf);
            outbuf.pop();
            console.log("PST", outbuf);

            return;
        }
        else if (tok.name === "ESCAPED_LITERAL") {
            return;
        }
        else if (tok.name === "STRING_SQUOTE_BEGIN") {
            if (lookahead.name === "STRING_SQUOTE_END") {
                // We do not copy empty strings to the output buffer.
                sqs_skip_token = true;
                return;
            }
        }
        else if (tok.name === "STRING_SQUOTE_END") {

        }
        else if (tok.name === "STRING_SQUOTE_CHAR") {

        }
        else if (tok.name === "STRING_DQUOTE_BEGIN") {
            if (lookahead.name === "STRING_DQUOTE_END") {
                // We do not copy empty strings to the output buffer.
                dqs_skip_token = true;
                return;
            }
        }
        else if (tok.name === "STRING_DQUOTE_END") {

        }
        else if (tok.name === "STRING_DQUOTE_CHAR") {

        }
        else if (tok.name === "SET") {

            if (in_env_var_name) {
                console.log("ERR: already in var assignment mode?");
            }

            set_startswith = lookahead.text;
            in_env_var_name = true;
        }
        else if (/^SET_DQUOTE_(?:BEGIN|CHAR|END)$/.test(tok.name)) {

            if (tok.name === "SET_DQUOTE_BEGIN" && varbuf.length === 0) {
                return;
            }

            if (tok.name === "SET_DQUOTE_END" && set_startswith === tok.text) {
                return;
            }

            if (in_env_var_name) {
                varbuf += tok.text;
            }
            else if (in_env_var_value) {
                valbuf += tok.text;
            }
        }
        else if (tok.name === "SET_ASSIGNMENT") {

            in_env_var_name = false;
            in_env_var_value = true;
        }
        else if (tok.name === "CALL" || tok.name === "END") {

            in_env_var_value = false;

            if (varbuf) {
                varmap[varbuf] = valbuf;
            }

            varbuf = "";
            valbuf = "";
        }
        else {
            console.log("?>", tok.name, tok.text);
        }
    });

    return {
        vars:    varmap,
        command: outbuf.join("")
    };
    }*/


module.exports = {

    filter: {
        widen_strings:       FILTER_slurp_literals_into_strings,
        strip_whitespace:    FILTER_strip_excessive_whitespace,
        strip_empty_strings: FILTER_strip_empty_strings
    },

    tokenise:    tokenise,
    split_command: split_command,
    parse: parse_cmdstr,
    strip_escape_chars: strip_escape_chars,
    expand_variables: expand_environment_variables
};
