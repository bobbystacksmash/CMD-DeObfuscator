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

    describe("Environment variables handling", () => {

        it("should expand environment variables introduced with set", () => {

            const input  = `SET foo=bar & echo %foo%`,
                  output = [`set foo=bar`, `echo bar`];

            assert.deepEqual(CMD.parse(input), output);
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
});
