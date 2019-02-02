const escapeRegexpString = require("escape-string-regexp");

// Variable Expander
// =================
//
// Command line parsing happens in multiple "phases", of which
// "percent expansion" is the first.  This is the process of
// identifiying and expanding environment variables, such as:
//
//   %comspec%                => C:\Windows\System32\cmd.exe
//   %comspec:cmd=powershell% => C:\Windows\System32\powershell.exe
//   %comspec:~-7%            => cmd.exe
//   %comspec:~11,8%          => system32
//   %comspec:~+11,+8%        => system32
//   %not_defined%            => %not_defined%
//
// During this phase we do not strip or handle escape or quote
// sequences.  Therefore, obfuscation such as: "%c^o^m^s^p^e^c%" will
// fail to lookup the value of %comspec% because the caret (^) escape
// symbol is not understood or stripped during this phase.
//


// Expanding Variables
// ===================
//
// When we talk about variable expansion, we're simply talking about
// replacing a variable (e.g.: "%COMSPEC%") with it's value:
// "C:\Windows\System32\cmd.exe"[1].  As discussed above, variables
// enclosed between percentage characters are expanded once, at "parse
// time", while variables enclosed between exclaimation marks are
// expanded at "runtime".
//
// [1] https://ss64.com/nt/delayedexpansion.html
//
const DEFAULT_ENV_VARS = {
    appdata: "C:\\Users\\whoami\\AppData\\Roaming",
    comspec: "C:\\Windows\\System32\\cmd.exe"
};

function to_num (num) {

    let ishex = (num) => /^[-+]?0x[a-f\d+]$/i.test("" + num),
        isoct = (num) => /^[-+]?0[0-8]+$/i.test("" + num);

    if (ishex(num)) {
        return parseInt(num, 16);
    }
    else if (isoct(num)) {
        return parseInt(num, 8);
    }
    else {
        return parseInt(num, 10);
    }
}


/**
 * Attempts to perform a find/replace with variable expansion against
 * a given command with values read from an optional variable
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
function var_find_replace (cmdstr, options) {

    const DEFAULTS = { sigil: "%" };
    options = options || {};
    options = Object.assign(DEFAULTS, options);

    const sigil           = options.sigil,
          find_replace_re = new RegExp(sigil + "([^:]*):([^\s]+)=([^\s]+)?" + sigil, "gi");

    let match = null;
    while ((match = find_replace_re.exec(cmdstr))) {

        let wholematch = match[0],
            findstr    = match[2],
            replstr    = match[3] || "",
            varname    = match[1].toLowerCase(),
            varvalue   = options.vars[varname];

        if (options.vars.hasOwnProperty(varname) === false) {
            continue;
        }

        let replaced_varvalue = varvalue.split(findstr).join(replstr);
        cmdstr = cmdstr.split(wholematch).join(replaced_varvalue);
    }

    return cmdstr;
}


/**
 * Given a command string, applies substring operation on the command,
 * returning an altered command string.  Substring operations come in
 * the following format:
 *
 *   %VAR:~[integer][,[integer]]%
 *
 * Either "integer" may have a leading plus (+) or minus (-) symbol.
 * Whitespace is valid in the following parts of the substr
 * expression:
 *
 *   %VAR:~\s*4,\s*3%
 *
 * @param {string} cmdstr - DOS command we wish to deobfuscate.
 * @param {Object} [vars] - An object mapping var names to values.
 *
 * @returns {string} An expanded form of `cmdstr` with all variable
 * find/replace operations performed.
 */
function var_substring (cmdstr, options) {

    options = options || {};
    options = Object.assign({ sigil: "%" }, options);

    const sigil     = options.sigil,
          substr_re = new RegExp(
              sigil + "([^:]*):~\\s*([+-]?[^,%]+)(?:,\\s*([+-]?[^,%]+))?" + sigil,
              "ig"
          );

    let replacements = [],
        substr_match = null;

    while ((substr_match = substr_re.exec(cmdstr))) {

        let var_name     = substr_match[1].toLowerCase(),
            var_value    = options.vars[var_name],
            substr_start = substr_match[2],
            substr_end   = substr_match[3];

        if (substr_start !== undefined) {
            substr_start = to_num(substr_start);
        }

        if (substr_end !== undefined) {
            substr_end = to_num(substr_end);
        }

        if (var_value === undefined) {
            continue;
        }
        else {
            var_value = var_value;
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
        cmdstr = cmdstr.replace(
            new RegExp(escapeRegexpString(r.find), "gi"),
            r.replace
        );
    });

    return cmdstr;
}


/**
 * Expands variables if wound within the given command string.
 *
 * @param {string} cmdstr - The command string to change.
 * @returns {string}
 */
function expand (cmdstr, options) {

    options = options || {};
    const DEFAULTS = {
        sigil: "%",
        enable_command_extensions: true
    };
    options = Object.assign({}, DEFAULTS, options);

    if (options.hasOwnProperty("vars")) {
        options.vars = Object.assign({}, DEFAULT_ENV_VARS, options.vars);
    }
    else {
        options.vars = DEFAULT_ENV_VARS;
    }

    // Standard Expansions
    // ===================
    //
    // The straightfoward %var% expansion routine.
    //
    Object.keys(options.vars).forEach(varname => {

        let varkey = Object.keys(options.vars).find(
            k => k.toLowerCase() === varname.toLowerCase()
        );

        if (!varkey) return;

        if (options.enable_command_extensions && /^.+:.+$/.test(varkey)) {
            // We cannot handle variables with a colon anywhere in the
            // name, with the exception of a variable with a single
            // colon at the very end of its name.
            return;
        }

        let searchre = new RegExp(
            escapeRegexpString(options.sigil + varkey + options.sigil),
            "gi"
        );

        let varvalue = options.vars[varkey];
        cmdstr = cmdstr.replace(searchre, varvalue);
    });

    if (options.enable_command_extensions == false) {
        return cmdstr;
    }

    // Find & Replace
    // ==============
    //
    // Searches the variable for all instances of STR, replacing with
    // REP, for example:
    //
    //   %foo:STR=REP%
    //   %foo:cat=dog%
    //
    cmdstr = var_find_replace(cmdstr, options);


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
    cmdstr = var_substring(cmdstr, options);

    return cmdstr;
};

module.exports = {
    expand: expand
};
