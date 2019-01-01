const assert = require("chai").assert,
      CMD    = require("../index");

describe("Command Deobfuscator: Integration Tests", () => {

    describe("Command Splitting", () => {

        it("should correctly split multiple commands in a command-string", () => {

            const input  = `echo foo & echo bar`,
                  output = [`echo foo`, `echo bar`];

            assert.deepEqual(CMD.parse(input), output);
        });
    });

    describe("Environment variable handling", () => {

        it("should not expand a variable which has been used before it was defined", () => {

            const input  = `echo %foo% & SET foo=bar`,
                  output = [`echo %foo%`, `SET foo=bar`];

            assert.deepEqual(CMD.parse(input), output);
        });

        it("should expand environment variables introduced with set", () => {

            const input  = `SET foo=bar & echo %foo%`,
                  output = [`SET foo=bar`, `echo bar`];

            assert.deepEqual(CMD.parse(input), output);
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
                    assert.deepEqual(CMD.parse(cmd), [`SET ${v}=foo`, `echo foo`])
                );
            });
        });

        it("should allow escaping of chars in the LHS of a SET expression", () => {
            assert.deepEqual(CMD.parse(`SET ^>=foo & echo %>%`), [`SET >=foo`, `echo foo`]);
            assert.deepEqual(CMD.parse(`SET ^^=foo & echo %^^%`), [`SET ^=foo`, `echo foo`]);
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

    describe("FireEye: Dosfuscation Report Examples", () => {

        it("should decode use of %COMSPEC% to echo 'SET'", () =>{

            const input  = `%comspec:~-16,1%%comspec:~-1%%comspec:~-13,1%`,
                  output = [`Set`];

            assert.deepEqual(CMD.parse(input), output);
        });
    });
});
