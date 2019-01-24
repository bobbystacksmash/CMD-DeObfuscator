const assert    = require("chai").assert,
      interpret = require("../../lib/interpreter");

const util = {
    filterEOF: (tokens) => tokens.filter(t => t.name !== "EOF"),
    lexemes:   (tokens) => util.filterEOF(tokens).map(t => t.text),
    names:     (tokens) => util.filterEOF(tokens).map(t => t.name)
};

describe("Interpreter", () => {

    describe("Variable Expansion", () => {

        it("should return an expanded command as a 1-element array", () => {

            const input  = `echo %comspec%`,
                  output = [`echo C:\\Windows\\System32\\cmd.exe`];
            assert.deepEqual(interpret(input), output);
        });
    });

    describe("De-obfuscation", () => {

        describe("Escapes", () => {

            it("should return the input without escapes", () => {
                const input  = `^p^o^w^e^r^s^h^e^l^l`,
                      output = ["powershell"];
                assert.deepEqual(interpret(input), output);
            });

            it("should disable escapes when 'strip_escapes' is false", () => {
                const input  = `^p^o^w^e^r^s^h^e^l^l`,
                      output = [input];
                assert.deepEqual(interpret(input, { strip_escapes: false }), output);
            });
        });

        describe("Strings", () => {

            it("should concatenate runs of regular strings and literals without delims", () => {
                const input  = `"abc"def"ghi"`,
                      output = [`"abcdefghi"`];
                assert.deepEqual(interpret(input), output);
            });

            it("should strip empty strings", () => {
                const input  = `w""scr""ipt""`,
                      output = [`wscript`];
                assert.deepEqual(interpret(input), output);
            });

            it("should unify strings or literals not separated by a delimiter", () => {

                const tests = [
                    {
                        input:  `w"s"cri"pt"`,
                        output: [`"wscript"`]
                    },
                    {
                        input:  `""cscr"ipt"`,
                        output: [`"cscript"`]
                     },
                    {
                        input:  `"a"b`,
                        output: [`"ab"`]
                    }
                ];

                tests.forEach(t => assert.deepEqual(interpret(t.input), t.output));
            });
        });
    });
});
