const assert = require("chai").assert,
      CMD    = require("../index");

const util = {

    debug: (tokens) => {
        tokens.forEach(token => {
            console.log(token);
        });
    },

    tokens: function (tokens) {
        return tokens.map(token => token.name);
    }
};

describe("Deobfuscator: Variable Expansion", () => {

    describe("Standard %ENVVAR% handling", () => {

        it("should replace variables for which there are definitions", () => {

            const input  = `%COMSPEC%`,
                  output = `C:\\Windows\\System32\\cmd.exe`;

            assert.equal(CMD.expand_variables(input), output);
        });

        it("should ignore variables for which there are no associations", () => {

            const input  = `I am %FOOBAR% things`,
                  output = `I am %FOOBAR% things`;

            assert.equal(CMD.expand_variables(input), output);
        });

        it("should replace multiple appearances of defined vars", () => {

            const input =  `a %b% c %b% d`,
                  output = `a XXX c XXX d`;

            assert.equal(CMD.expand_variables(input, { b: "XXX" }), output);
        });
    });

    describe("Variable substring operations", () => {

        it("should leave the string unchanged if VAR is not defined", () => {

            const input  = `%FOO:~3%`,
                  output = `%FOO:~3%`;

            assert.equal(CMD.expand_variables(input), output);
        });

        it("should replace the whole value when using %VAR:~0%", () => {

            const input  = `%FOO:~0%`,
                  output = `abcdef`;

            assert.equal(CMD.expand_variables(input, { foo: "abcdef" }), output);
        });

        it("should return the correct substr when given: :~3%", () => {

            const input  = `%FOO:~3%`,
                  output = `def`;

            assert.equal(CMD.expand_variables(input, { foo: "abcdef" }), output);
        });

        it("should handle both + and - before substr indexes", () => {

            const tests = [
                { input: `%FOO:~+3%`,    output: "def" },
                { input: `%FOO:~+3,+2%`, output: "de" }
            ];

            tests.forEach(T => {
                assert.equal(CMD.expand_variables(T.input, { foo: "abcdef" }), T.output);
            });
        });

        it("should handle whitespace between parts of the substr expression", () => {

            const tests = [
                { input: `%FOO:   ~  3%`,    output: "def" },
            ];

            tests.forEach(T => {
                assert.equal(CMD.expand_variables(T.input, { foo: "abcdef" }), T.output);
            });
        });

        it("should return the correct substr when given upper and lower bounds: :~n,N%", () => {

            const vars = { foo: "abcdef", bar: "123456789abcdef0" };

            const tests = [
                { input: `%FOO:~0,3%`, output: "abc" },
                { input: `%FOO:~1,3%`, output: "bcd" },
                { input: `%FOO:~3,1%`, output: "d"   },
                { input: `%FOO:~3,9%`, output: "def" },

                // Negatives
                { input: `%FOO:~-3%`, output: "def" },

                // Extract everything except the last 2 characters
                { input: `%FOO:~0,-2%`, output: "abcd" },

                // Extract the 5th and 6th chars from the end
                { input: `%BAR:~-7,-5%`, output: "ab" }
            ];

            tests.forEach(T => {
                assert.equal(CMD.expand_variables(T.input, vars), T.output);
            });
        });

        it("should correctly replace a (negative,positive) substr combination", () => {

            const vars  = { foo: "abcdef" },
                  tests = [
                      { input: `%FOO:~-3,1%`, output: "d" },
                  ];

            tests.forEach(t => {
                assert.equal(CMD.expand_variables(t.input, vars), t.output);
            });
        });

        it("should correctly replace multiple environment variables in one cmd", () => {

            const input = `%abc%%def%%ghi%%j%`,
                  vars  = {
                      abc: "pow",
                      def: "ers",
                      ghi: "hel",
                      j:  "l"
                  };

            assert.equal(CMD.expand_variables(input, vars), "powershell");
        });
    });

    describe("Delayed expansion of variables", () => {

        xit("xx", () => {

            const input = `set _var=first`;

            console.log(CMD.expand_variables(input));
            assert.isTrue(false);
        });

    });

    describe("%COMSPEC% specific tests from FireEye DOSFuscation whitepaper", () => {

        // See page. 14 of FireEye's DOSFuscation whitepaper for details.
        it("should always produce the full %COMSPEC% var for the following substrs", () => {

            const COMSPEC = `C:\\Windows\\System32\\cmd.exe`,
                  vars    = { comspec: COMSPEC };

            const tests = [
                `%COMSPEC:~0%`,
                `%COMSPEC:~0,27%`,
                `%COMSPEC:~-27%`,
                `%COMSPEC:~-27,27%`,
                `%COMSPEC:~0,1337%`,
                `%COMSPEC:~-1337%`,
                `%COMSPEC:~-1337,1337%`
            ];

            tests.forEach(input => {
                assert.equal(CMD.expand_variables(input, vars), COMSPEC);
            });
        });
    });

    describe("Variable character replacement", () => {

        it("should handle replacing all occurances of a given char within a variable", () => {

            const input  = `%FOO:cat=dog%`,
                  msg    = `the cat sat cat on cat the cat mat`,
                  vars   = { foo: msg };
            assert.equal(CMD.expand_variables(input, vars), msg.replace(/cat/g, "dog"));
        });

        it("should handle removing all matching chars from a given variable", () => {

            const input  = `%x:@=%`,
                  output = `wscript`,
                  vars   = { x: `wsc@ript` };

            assert.equal(CMD.expand_variables(input, vars), `wscript`);
        });

        it("should expand the variable without find/replace when find does not match", () => {

            const input  = `%x:z=a%`,
                  output = `foobar`,
                  vars   = { x: output };

            assert.equal(CMD.expand_variables(input, vars), output);
        });
    });
});
