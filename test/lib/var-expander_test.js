const assert   = require("chai").assert,
      expander = require("../../lib/var-expander").expand;

const VARS = {
    hello  : "world",
    "foo:" : "bar",
    UP     : "DOWN",
    "'"    : "quote"
};

describe("Phase 1: Percent Expansion", () => {

    describe("Basic Expansions", () => {

        it("should expand defined variables, regardless of case", () => {
            Object.keys(VARS).forEach(_var => {
                let v = "%" + _var + "%";
                assert.deepEqual(expander(v, { vars: VARS}), VARS[_var]);
            });

            Object.keys(VARS).forEach(_var => {
                let v = "%" + _var + "%";
                assert.deepEqual(expander(v.toUpperCase(), { vars: VARS}), VARS[_var]);
            });

            Object.keys(VARS).forEach(_var => {
                let v = "%" + _var + "%";
                assert.deepEqual(expander(v.toLowerCase(), { vars: VARS}), VARS[_var]);
            });
        });

        it("should not expand escaped variables which are defined", () => {

            let vars = { "foo": "bar" },
                cmd  = "%f^o^o%";

            assert.deepEqual(expander(cmd, { vars: vars }), cmd);
        });

        it("should only expand the inner part of a double %% var", () => {
            //
            // Example:
            //
            //   %%foo%% => %bar% when "foo=bar"
            //
            Object.keys(VARS).forEach(_var => {
                let v = "%%" + _var + "%%";
                assert.deepEqual(expander(v, { vars: VARS }), "%" + VARS[_var] + "%");
            });
        });

        it("should not expand variable which are not defined", () => {

            let undef_vars = ["%alpha%", "%bravo%", "%charlie%"];

            undef_vars.forEach(_var => {
                assert.deepEqual(expander(_var), _var);
            });
        });
    });

    describe("Command Extensions", () => {

        describe("Find & Replace", () => {

            it("should perform string substitution when the var exists", () => {

                const vars   = { hello: "world" },
                      input  = "%hello:d=x%",
                      output = "worlx";

                assert.deepEqual(expander(input, { vars: vars }), output);
            });

            it("should leave the varspec unchanged when the var is not defined", () => {

                const input  = "%hello=d:x%",
                      output = input; // it's unchanged.

                assert.deepEqual(expander(input), output);
            });

            it("should replace more than one char if specified", () => {

                const input  = "hi %hello:world=planet%.",
                      output = "hi planet.";

                assert.deepEqual(expander(input, { vars: { hello: "world" } }), output);
            });
        });

        describe("Substrings", () => {

            it("should support basic substring operations", () => {

                const tests = [
                    {
                        input: "%comspec:~-7%", output: "cmd.exe"
                    },
                    {
                        input: "%CoMsPec:~-7,3%", output: "cmd"
                    },
                    {
                        input: "%comspec:~3%", output: "Windows\\System32\\cmd.exe"
                    },
                    {
                        input: "%comspec:~   +3,+7%", output: "Windows"
                    },
                    {
                        input: "%comspec:~   +3, +7%", output: "Windows"
                    },
                    {
                        input: "%comspec: ~   +3, +7%", output:  "%comspec: ~   +3, +7%"
                    }
                ];

                tests.forEach(t => {
                    assert.deepEqual(expander(t.input), t.output);
                });
            });

            it("should identify hex and octal range numbers", () => {

                const tests = [
                    { input: "%comspec:~0xB%", output: "System32\\cmd.exe" },
                    { input: "%comspec:~0xB,6%", output: "System" },
                    { input: "%comspec:~0XB,0x6%", output: "System" },
                    { input: "%comspec:~013,6%", output: "System" },
                    { input: "%comspec:~013,0xC%", output: "System32\\cmd" },
                    { input: "%comspec:~-0xA%", output: "32\\cmd.exe" }
                ];

                tests.forEach(t => assert.deepEqual(expander(t.input), t.output));
            });

            it("should replace the whole value when using %VAR:~0%", () => {

                const opts = {
                    vars: {
                        foo: "bar",
                        hello: "world"
                    }
                };

                Object.keys(opts.vars).forEach(_var => {
                    let v = "%" + _var + ":~0%";
                    assert.deepEqual(expander(v, opts), opts.vars[_var]);
                });
            });

            it("should not expand vars with a ':' in the name when cmd extensions are enabled", () => {

                // Variables which contain ':' in the name cannot be
                // expanded unless command extensions are disabled.
                // The only exception to this is when a variable name
                // contains a single colon at the end of its name.
                // These variables will work without disabling command
                // extensions, however substring or find/replace
                // operations are not permitted.  For details, see:
                //
                // https://stackoverflow.com/questions/4094699/how-does-the-windows-command-interpreter-cmd-exe-parse-scripts/7970912#7970912
                const opts = {
                    vars: {
                        "foo:": "bar",
                        "h:ello": "world"
                    }
                };

                assert.deepEqual(
                    expander("%foo:%", opts),
                    opts.vars["foo:"],
                    "Expansions are fine when varname ends with a ':'"
                );

                assert.deepEqual(expander("%foo::~2%"), "%foo::~2%");
                assert.deepEqual(
                    expander("%foo::~2%"),
                    "%foo::~2%",
                    "Expansions not performed for substr operations"
                );

                assert.deepEqual(
                    expander("%foo:a=x%", opts),
                    "%foo:a=x%",
                    "Expansions not performed for find/replace operations"
                );

                assert.deepEqual(
                    expander("%h:ello%", opts),
                    "%h:ello%",
                    "Expansions not performed when var contains a colon midway through"
                );

                // Disable extensions!
                assert.deepEqual(
                    expander("%h:ello%", { enable_command_extensions: false, vars: opts.vars }),
                    opts.vars["h:ello"]
                );

                // Finally, what about the same expression but with
                // command extensions disabled?
                //
                //
            });
        });
    });
});
