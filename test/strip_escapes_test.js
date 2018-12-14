const assert       = require("chai").assert,
      deobfuscator = require("../index");

const util = {

    tokens: function (tokens) {
        return tokens.map(token => token.name);
    }
};

describe("DeObfuscator: Strip Escapes", () => {

    it("should remove escape sequences when used between double quotes", () => {

        const input  = `SET "foo=b^a^r"`,
              output = `SET "foo=bar"`;

        assert.equal(deobfuscator.strip_escape_chars(input), output);
    });

    it("should leave an escaped caret (^) when double escaped (^^)", () => {

        const input  = `b^^ar`,
              output = `b^ar`;

        assert.equal(deobfuscator.strip_escape_chars(input), output);
    });
});
