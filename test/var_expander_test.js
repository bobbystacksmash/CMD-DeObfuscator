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
