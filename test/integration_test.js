const assert = require("chai").assert,
      CMD    = require("../index");

describe("Command Deobfuscator: Integration Tests", () => {

    describe("Command Splitting and Identification", () => {

        it("should correctly split multiple commands in a command-string", () => {

            const input  = `echo foo & echo bar`,
                  output = [`echo foo`, `echo bar`];

            assert.deepEqual(CMD.parse(input), output);
        });

        describe("CMD handling", () => {

            describe("CMD.EXE sub-execing itself", () => {

                it("should correctly identify CMD.EXE sub/sub/sub execs", () => {

                    const command  = `cmd /c c^m^d; calc.exe`,
                          expected = ["calc.exe"];

                    assert.deepEqual(CMD.parse(command), expected);
                });
            });

            describe("Delayed Expansion (DE)", () => {

                it("should not perform delayed expansion by default", () => {

                    const input  = `Set "foo=bar" & echo !foo!`,
                          output = [`Set "foo=bar"`, `echo !foo!`];

                    assert.deepEqual(CMD.parse(input), output);
                });
            });
        });
    });

        // TODO: add other tests, like 'foo && bar', 'foo || bar', etc.
    /*it("should perform command-specific handling for certain (known) commands", () => {

            const tests = [
                { input: `cmd "set foo=bar & echo %foo%"`, output: [`set foo=bar`, `echo bar`] },
                { input: `cmd calc.exe`, output: [`calc.exe`] },
                { input: `cmd cmd cmd cmd calc.exe`, output: [`calc.exe`] }
            ];

            tests.forEach(t => assert.deepEqual(CMD.parse(t.input), t.output));
        });
    });*/

    describe("Environment variable handling", () => {

        it("should expand environment variables that are already defined", () => {

            const input  = `%COMSPEC%`,
                  output = `C:\\Windows\\System32\\cmd.exe`;

            assert.equal(CMD.parse(input, { expand_vars: true }), output);
        });

        it("should not expand environment variables (delayed expansion is off)", () => {

            const input  = `set "foo=bar" & echo %foo%`,
                  output = [`set "foo=bar"`, `echo %foo%`];

            assert.deepEqual(CMD.parse(input), output);
        });

        it("should not expand a variable which has been used before it was defined", () => {

            const input  = `echo %foo% & SET foo=bar`,
                  output = [`echo %foo%`, `SET foo=bar`];

            assert.deepEqual(CMD.parse(input), output);
        });

        it("should handle delayed expansion via the expand_vars flag", () => {

            const input  = `SET foo=bar & echo %foo%`,
                  tests = [
                      {
                          output: [`SET foo=bar`, `echo %foo%`],
                          expand: false
                      },
                      {
                          output: [`SET foo=bar`, `echo bar`],
                          expand: true
                      }
                  ];

            tests.forEach(t => assert.deepEqual(CMD.parse(input, {expand_vars: t.expand}), t.output));
        });

        it("should allow many ASCII chars on the LHS of a SET expression", () => {

            const var_names = [
                `%`,
                `%@!`,
                `_`,
                `foo@bar`,
                `##`,
            ];

            var_names.forEach(v => {
                let cmd = `SET ${v}=foo & echo %${v}%`;
                assert.deepEqual(
                    assert.deepEqual(
                        CMD.parse(cmd, { expand_vars: true }), [`SET ${v}=foo`, `echo foo`]
                    )
                );
            });
        });

        it("should allow escaping of chars in the LHS of a SET expression", () => {
            assert.deepEqual(
                CMD.parse(`SET ^>=foo & echo %>%`, { expand_vars: true }),
                [`SET >=foo`, `echo foo`]
            );

            assert.deepEqual(
                CMD.parse(`SET ^^=foo & echo %^^%`, { expand_vars: true }),
                [`SET ^=foo`, `echo foo`]
            );
        });
    });

    describe("Noise reduction", () => {

        it("should remove empty double-quoted strings", () => {

            const tests = [
                [`c""alc.exe`, [`calc.exe`]]
            ];

            tests.forEach(test => assert.deepEqual(CMD.parse(test[0]), test[1]));
        });

        it("should expand strings to enclose surrounding literals", () => {

            const tests = [
                [`h"t"t"p"`, [`"http"`]]
            ];

            tests.forEach(test => assert.deepEqual(CMD.parse(test[0]), test[1]));
        });

        it(`should sanitise a string mixing empty quotes ("") and "qu"oted"chars"`, () => {

            const tests = [
                [`w""sc"r"i"p"t`, [`"wscript"`]]
            ];

            tests.forEach(test => assert.deepEqual(CMD.parse(test[0]), test[1]));
        });

        it("should strip excessive escape sequences", () => {

            const input  = `p^o^w^e^r^s^h^e^l^l`,
                  output = `powershell`;

            assert.deepEqual(CMD.parse(input), [output]);
        });

        it("should ignore escapes in double-quoted strings", () => {

            const input  = `"p^o^w^e^r^s^h^e^l^l"`,
                  output = `"p^o^w^e^r^s^h^e^l^l"`;

            assert.deepEqual(CMD.parse(input), [output]);
        });

        it("should strip escapes from single-quoted strings", () => {

            const input  = `'p^o^w^e^r^s^h^e^l^l'`,
                  output = `'powershell'`;

            assert.deepEqual(CMD.parse(input), [output]);
        });
    });

    describe("Deobfuscation tests", () => {

        it("should correctly identify a cmd launching an obfuscated cmd", () => {

            // TODO: finish implementing this.

            const input = `"C:\\Windows\\System32\\cmd.exe" ` +
                          `c^m^d /V "set foo=bar & echo !foo!"`,
                  output = ["set foo=bar", "echo bar"];

            assert.deepEqual(CMD.parse(input), output);
        });
    });

    describe("FireEye: Dosfuscation Report Examples", () => {

        it("should decode use of %COMSPEC% to echo 'SET'", () =>{

            const input  = `%comspec:~-16,1%%comspec:~-1%%comspec:~-13,1%`,
                  output = [`Set`];

            assert.deepEqual(CMD.parse(input, { expand_vars: true }), output);
        });

        it("should allow setting a single quote ' to some value", () => {

            const input  = `SET '=abc & echo %'%`,
                  output = [`SET '=abc`, `echo abc`];

            assert.deepEqual(CMD.parse(input, { expand_vars: true }), output);
        });
    });
});
