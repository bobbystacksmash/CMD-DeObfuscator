const JisonLex           = require("jison-lex"),
      path               = require("path").win32,
      fs                 = require("fs"),
      escapeRegexpString = require("escape-string-regexp");

const grammar = fs.readFileSync(require.resolve("./comspec.l")).toString(),
      lexer   = new JisonLex(grammar);

// ;;;;;;;;;;;;;;;;;;;;;;;
// ;; Utility functions ;;
// ;;;;;;;;;;;;;;;;;;;;;;;
//
function stringify_tokens(tokens) {
    return tokens.map(t => t.text).join("");
}
function has (obj, key) {
    return obj.hasOwnProperty(key);
}

function hasAll (obj, keys) {
    return keys.every(k => obj.hasOwnProperty(k));
}

function hasAny (obj, keys) {
    return keys.some(k => obj.hasOwnProperty(k));
}

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
//
// Identified Command
// ==================
/**
 * @typedef {Object} IdentifiedCommand
 * @property {string} command - The name of the command ("" if unknown).
 * @property {number} offset  - The offset of where the command identifier
 *                              ends in the tokens array.
 */


/**
 * Command dispatch table maps commands we can handle to functions
 * which do the command handling.  Generally, when we're talking about
 * "handling a command", we mean altering the parsed token stream so
 * that the tokens are correctly altered based upon the command
 * handling them.  For example, we may have been passed something
 * like:
 *
 *   cmd "set foo=bar"
 *
 * The token stream for this command will look something like:
 *
 *   LIT(c), LIT(m), LIT(d), STR("), STRLIT(s), STRLIT(e), ...
 *
 * If we remove the leading "cmd " from the string, and strip the
 * surrounding double quotes from the command and re-tokenise it,
 * we'll get a new tokenised sequence which we can handle.
 *
 */
const cmd_dispatch = {
    cmd: FILTER_handle_cmd
};

/**
 * Provides specific command clean-up for the 'cmd.exe' command.
 *
 * @param {IdentifiedCommand} ident - The identified command object.
 * @param {Tokens|Array} tokens - The array of tokens from parsing the cmd.
 *
 * @returns {Tokens|Array}
 */
function FILTER_handle_cmd (ident, tokens) {

    tokens = Array.prototype.slice.call(tokens);

    if (ident.offset >= tokens.length) {
        return {
            tokens: tokens,
            switches: {},
            finished: true
        };
    }

    let cmd = tokens
            .slice(ident.offset)
            .map(t => t.text)
            .join("")
            .replace(/^\s+/,  "");

    // A note about argument parsing
    // =============================
    //
    // The CMD.EXE help page says that the syntax of the 'cmd.exe'
    // command is:
    //
    //   CMD [charset] [options] [/C command]
    //   CMD [charset] [options] [/K command]
    //
    // For example:
    //
    //   CMD /V:on "set foo=bar& calc.exe"
    //
    //
    // The important part is the location of the first dquote, which
    // tells us where the COMMAND part of the line begins.  We capture
    // the location of this char (if exists), and use its string
    // offset as the point at which we stop looking for command
    // switches.
    //
    const first_dquote_offset = cmd.split("").findIndex(chr => chr === '"'),
          switch_re           = /\/([A-Z])([:][^\s]+)?(?:$|\s)/ig;

    let match             = undefined,
        last_match_offset = undefined,
        switches          = {
            delayed_expansion: false
        };

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

    while ((match = switch_re.exec(cmd))) {

        let wholematch       = match[0],
            _switch          = match[1],
            _value           = "",
            match_end_offset = match[0].length + match.index;

        last_match_offset = match_end_offset;

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
        }

        switches[_switch] = _value;

        if (match_end_offset && (match_end_offset > first_dquote_offset)) {
            break;
        }
    }

    // Now we've finished parsing the command arguments, we can strip
    // the args leaving only the next part of the command string.
    if (last_match_offset !== undefined) {
        cmd = cmd.substr(last_match_offset);
    }

    // If the remaining command part starts and ends with a double
    // quote, we strip them.
    cmd = cmd.replace(/^\"|\"$/g, "");

    return {
        tokens: tokenise(cmd),
        switches: switches,
        finished: false
    };
}

/**
 * Parses a given command string in to individual commands, before
 * applying expansion and de-obfuscation filters to each command.
 *
 * @param {string} cmdstr - The original command string to be
 * de-obfuscated.
 *
*/
function parse_cmdstr (cmdstr, options) {

    const DEFAULTS = {
        delayed_expansion: false,
        expand_inline:     false,
        vars: {}
    };

    options = options || {};
    options = Object.assign({}, DEFAULTS, options);

    let collector = { vars: {}, switches: {}, output: [] };

    cmdstr = expand_environment_variables(cmdstr, options.vars);

    (function parse_cmdstr_rec (cmdstr, switches) {

        switches = switches || {};

        split_command(cmdstr).forEach(cmd => {

            let result = interpret_command(cmd);
            collector.vars = Object.assign(collector.vars, result.vars);

            if (result.ident.finished) {
                collector.output.push(stringify_tokens(result.ident.tokens));
            }
            else if (result.ident.command === "cmd") {
                //
                // NOTES ON DELAYED EXPANSION
                // ==========================
                //
                // We have support for delayed expansion.  Tests on
                // Win7 and Win10 hosts show that delayed expansion is
                // only enabled for the current CMD context, for
                // example, given the following command:
                //
                //   cmd /V "set foo=bar& echo !foo!"
                //
                // The output will be "echo bar" because delayed
                // expansion is set.  However, it does not cascade in
                // to lower-down CMD instances, for example:
                //
                //   cmd /V "cmd \"set foo=bar& echo !foo!\""
                //
                // This will produce "echo !foo!" because we created a
                // sub-cmd context, and the default was applied.
                //
                let new_cmd = stringify_tokens(result.ident.tokens);

                if (new_cmd.toLowerCase() !== "cmd") { // infinite loop protection.
                    parse_cmdstr_rec(
                        stringify_tokens(result.ident.tokens),
                        result.ident.switches
                    );
                }
            }
            else {

                let delayed_exp = Object.assign({}, options, switches).delayed_expansion;

                // We do not want to expand percentage vars as
                // that time has passed.
                cmd = expand_environment_variables(
                    result.clean,
                    collector.vars,
                    {
                        expand_percent_vars: options.expand_inline,
                        delayed_expansion: delayed_exp
                    }
                );

                collector.output.push(cmd);
            }
        });
    }(cmdstr));

    return collector.output;
}

/**
 * Given an array of Token objects, attempts to identify the command
 * being run.  If a command is found, an IdentifiedCommand object is
 * returned which will contain both the command name and the offset
 * from where abouts in the tokens array the command string ends.  If
 * the command cannot be found, returns an empty name ("") and -1 for
 * the offset.
 *
 * For best results, this command should be called AFTER all filtering
 * has taken place, thus ensuring the command is in the least
 * obfuscated state BEFORE attempting command identification.
 *
 * @param {Token|Array} tokens - The command string to analyse.
 * @returns {IdentifiedCommand}
 */
function try_identify_command (tokens) {

    tokens = Array.prototype.slice.call(tokens);

    let identified_command = {
        command : "",
        switches: {},
        offset  : -1
    };

    /*
     * Double-Quoted commands
     * ======================
     *
     * For example, matches something similar to:
     *
     *   "C:\Windows\System32\cmd.exe"
     *   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     */
    if (tokens[0].name === "STRING_DQUOTE_BEGIN") {

        let dquote_end_index = tokens.findIndex(
            (t, i) => i > 0 && t.name === "STRING_DQUOTE_END"
        );

        if (!dquote_end_index) {
            // We can't do much if the CMD doesn't have an ending
            // DQUOTE.  Bad command.
            return identified_command;
        }

        let cmd = tokens
                .splice(0, dquote_end_index)
                .map(tok => tok.text)
                .join("")
                .replace(/^\"|\"$/g, "");

        identified_command.command = path.basename(cmd).replace(/\.exe$/i, "");
        identified_command.offset  = dquote_end_index + 1;
    }
    else if (tokens[0].name === "SET") {
        identified_command.command = "set";
        identified_command.offset  = 1;
    }
    else {

        let end_index = tokens.findIndex(t => (t.text === " " || t.name === "SEMICOLON"));
        end_index = (end_index < 0) ? tokens.length : end_index;

        let cmd = tokens
                .splice(0, end_index)
                .map(tok => tok.text)
                .join("");

        if (/[\\/]/.test(cmd) || /^[a-z]:/i.test(cmd)) {
            // If the path contains path separators, or some drive
            // identifier such as 'C:', then clean-up the path and
            // return the command.
            identified_command.command = path.basename(cmd).replace(/\.exe$/i, "");
            identified_command.offset  = end_index + 1;
        }
        else if (cmd) {
            identified_command.command = cmd.replace(/\.exe$/i, "");
            identified_command.offset  = end_index;
        }
    }

    return identified_command;
}

/**
 * Given an array of Token objects, attempts to remove all non-quoted
 * contiguous whitespace LITERALS, leaving a single space between each
 * word boundary.
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
                i = 0;
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
 * Given an array of Tokens, attempts to remove all unnecessary commas
 * from the tokenised sequence.
 *
 * @param {Token|Array} tokens - An array of tokens.
 * @returns {Token|Array}
 */
function FILTER_strip_commas (tokens) {
    return tokens.filter(token => token.name !== "COMMA");
}

/**
 * Given an array of Tokens, attempts to fix-up all tokens which were
 * previously escaped tokens.
 *
 * @param {Token|Array} tokens - An array of tokens.
 * @returns {Token|Array}
 */
function FILTER_apply_escapes (tokens) {

    let filtered = tokens
        .filter(tok => tok.name !== "ESCAPE")
        .map((tok, i, tokens) => {
            if (tok.name === "ESCAPED_LITERAL") tokens[i].name = "LITERAL";
            return tokens[i];
        });

    return filtered;
}

/**
 * Given a command string, attempts to partially interpret the
 * command, returning an object which can be used to present the
 * command in an easy-to-understand way.
 *
 * @param {string} cmdstr - The command to run/parse.
 * @returns {Object}
 */
function interpret_command (cmdstr) {

    let clean_cmdstr = cmdstr.replace(/^\s+|\s+$/, "");

    // Parse the command string in to an array of Token objects.
    let tokens = tokenise(clean_cmdstr),
        ident  = try_identify_command(tokens);

    if (cmd_dispatch.hasOwnProperty(ident.command)) {
        let handled = cmd_dispatch[ident.command](ident, tokens);
        ident.tokens   = handled.tokens;
        ident.switches = handled.switches;
        ident.finished = handled.finished;
    }

    let flags  = {
            in_set_cmd              : false,
            capturing_env_var_name  : false,
            capturing_env_var_value : false
        };

    let env_vars      = {},
        env_var_name  = "",
        env_var_value = "";

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
            break;

        case "ESCAPED":
            break;

        case "SET":
            flags.capturing_env_var_name = true;
            flags.in_set_cmd             = true;
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

        if (/^%[^%]+[^%]%$/.test(env_var_name)) {
            // Special handling for the case where someone sets:
            //
            //   SET %foo%=bar
            //
            // In this case, '%foo%' is treated as 'foo'.  This is
            // different from something like:
            //
            //   SET %%foo%%=bar
            //
            // which Windows treats as '%%foo%%' which is !== '%foo%'.
            //
            env_var_name = env_var_name.replace(/^%|%$/g, "");
        }

        env_vars[env_var_name] = {
            first: env_var_value,
            curr: env_var_value
        };
    }

    return {
        ident: ident,
        clean: outbuf.join(""),
        vars: env_vars
    };
}

/**
 * Given a command string, attempts to split the string, returning an
 * array of individual command strings.
 *
 * @param {string} command - a CMD.EXE command.
 * @returns {Tokens|Array} Each command is an element in the array.
 */
function split_command (command_str) {

    let tokens   = tokenise(command_str, { filter: false }),
        index    = 0,
        commands = [""];

    tokens.forEach(tok => {

        if (/^(?:CALL|COND_CALL|SEMICOLON)$/.test(tok.name)) {
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
    options = Object.assign({}, { escapes_as_literals: false, filter: true }, options);

    lexer.setInput(cmdstr);

    let tokens = [];

    while (true) {
        let token = lexer.lex();
        if (token === "EOF") break;

        if (options.escapes_as_literals) {
            if (token.name === "ESCAPE") {
                token.name = "LITERAL";
            }
            else if (token.name === "ESCAPED_LITERAL") {
                if (token.text === "=") {
                    token.name = "SET_ASSIGNMENT";
                }
                else {
                    token.name = "LITERAL";
                }
            }
        }

        tokens.push(token);
    }

    if (options.filter) {
        tokens = FILTER_apply_escapes(tokens);
        tokens = FILTER_strip_empty_strings(tokens);
        tokens = FILTER_slurp_literals_into_strings(tokens);
        tokens = FILTER_strip_excessive_whitespace(tokens);
        //tokens = FILTER_strip_commas(tokens);
    }

    let cleancmd = stringify_tokens(tokens);

    if (cmdstr !== cleancmd) {
        return tokenise(cleancmd, { escapes_as_literals: true });
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

    let find_replace_re = /%([^:]*):([^\s]+)=([^\s]+)?%/ig,
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

        let replaced_varvalue = varvalue.first.split(findstr).join(replstr);

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
function expand_environment_variables (cmdstr, vars, options) {

    options = options || {};
    const defaults = {
        expand_percent_vars: true,
        delayed_expansion: false
    };
    options = Object.assign({}, defaults, options);

    const default_vars = {
        appdata: {
            first: `C:\\Users\\whoami\\AppData\\Roaming`,
            curr:  `C:\\Users\\whoami\\AppData\\Roaming`
        },
        comspec: {
            first: `C:\\Windows\\System32\\cmd.exe`,
            curr:  `C:\\Windows\\System32\\cmd.exe`
        }
    };

    if (vars) {
        Object.keys(vars).forEach(varname => {
            const varvalue = vars[varname];
            if (typeof varvalue === "string") {
                vars[varname] = {
                    first: varvalue,
                    curr:  varvalue
                };
            }
        });
    }
    vars = Object.assign(default_vars, vars);

    // Expand Variables
    // ================
    //
    // Take all instances of '%foo%' and replace with the value found
    // within the 'vars' dict.
    //
    let cmd = cmdstr;

    if (options.expand_percent_vars) {
        Object.keys(vars).forEach(varname => {
            cmd = cmd.replace(
                new RegExp(escapeRegexpString(`%${varname}%`), "gi"), vars[varname].first
            );
        });
    }

    // Delayed Expansion
    // =================
    //
    // Instead of '%foo%', delayed expansion works with '!foo!', and
    // reads from '.curr' instead of '.first'.
    //
    if (options.delayed_expansion) {
        Object.keys(vars).forEach(varname => {
            cmd = cmd.replace(
                new RegExp(escapeRegexpString(`!${varname}!`), "gi"), // find
                vars[varname].curr                                    // replace
            );
        });
    }

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
    let substr_re    = /%([^:]*):\s*~\s*([+-]?\d+)(?:,([+-]?\d+))?%/ig,
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
        else {
            var_value = var_value.first;
        }

        let replace = {
            find: substr_match[0]
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
                /*
                 * Handles cases such as: %foo:~-10,3%.
                 */
                let substr_offset = (substr_end + substr_start) * -1;
                replace.replace = rev((rev(var_value).substr(substr_offset, substr_end)));
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

    replacements.forEach(r => {
        cmd = cmd.replace(new RegExp(escapeRegexpString(r.find), "gi"), r.replace);
    });

    return cmd;
}

module.exports = {

    filter: {
        widen_strings:       FILTER_slurp_literals_into_strings,
        strip_escapes:       FILTER_apply_escapes,
        strip_whitespace:    FILTER_strip_excessive_whitespace,
        strip_empty_strings: FILTER_strip_empty_strings,
        strip_commas:        FILTER_strip_commas,

        // Command handlers
        handle_CMD: FILTER_handle_cmd,
    },

    try_identify_command: try_identify_command,

    tokenise:    tokenise,
    split_command: split_command,
    parse: parse_cmdstr,
    expand_variables: expand_environment_variables
};
