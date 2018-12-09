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

        const input    = `%COMSPEC%`,
              expected = `C:\\Windows\\System32\\cmd.exe`;

        assert.equal(deobfuscator.expand_variables(input), expected);
    });
});
