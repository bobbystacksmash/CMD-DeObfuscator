const assert       = require("chai").assert,
      deobfuscator = require("../index");

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

describe("DeObfuscator: Variable Expansion", () => {

    describe("Standard %ENVVAR% handling", () => {

        it("should replace variables for which there are definitions", () => {

            const input  = `%COMSPEC%`,
                  output = `C:\\Windows\\System32\\cmd.exe`;

            assert.equal(deobfuscator.expand_variables(input), output);
        });

        it("should ignore variables for which there are no associations", () => {

            const input  = `I am %FOOBAR% things`,
                  output = `I am %FOOBAR% things`;

            assert.equal(deobfuscator.expand_variables(input), output);
        });

        it("should replace multiple appearances of defined vars", () => {

            const input =  `a %b% c %b% d`,
                  output = `a XXX c XXX d`;

            assert.equal(deobfuscator.expand_variables(input, { b: "XXX" }), output);
        });
    });

    describe("Variable substring operations", () => {

        it("should leave the string unchanged if VAR is not defined", () => {

            const input  = `%FOO:~3%`,
                  output = input;

            assert.equal(deobfuscator.expand_variables(input), output);
        });

        it("should replace the whole value when using %VAR:~0%", () => {

            const input  = `%FOO:~0%`,
                  output = `abcdef`;

            assert.equal(deobfuscator.expand_variables(input, { foo: "abcdef" }), output);
        });

        it("should apply substring when VAR is defined", () => {

            const input  = `%FOO:~3%`,
                  output = `def`;

            assert.equal(deobfuscator.expand_variables(input, { foo: "abcdef" }), output);
        });
    });
});
