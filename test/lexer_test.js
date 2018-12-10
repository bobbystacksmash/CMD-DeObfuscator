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

describe("DeObfuscator Tests", () => {
    describe("Quotes", () => {

        it("should correctly detect an empty string", () => {

            const input = `""`,
                  expected = [
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), expected);
        });

        it("should detect an empty string between literals", () => {

            const input = `po""wer`,
                  expected = [
                      "LITERAL",             // p
                      "LITERAL",             // o
                      "STRING_DQUOTE_BEGIN", // ""
                      "STRING_DQUOTE_END",   // ""
                      "LITERAL",             // w
                      "LITERAL",             // e
                      "LITERAL"              // r
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), expected);
        });

        it("should correctly detect balanced double-quotes ", () => {

            const input    = `"abc"`,
                  expected = [
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), expected);
        });

        it("should handle empty double-quotes within a string", () => {

            const input = `""a""b`,
                  expected = [
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_END",
                      "LITERAL",
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_END",
                      "LITERAL"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), expected);
        });
    });

    describe("Escapes", () => {

        it("should detect the escape symbol", () => {

            const input    = `a^b^c`,
                  expected = [
                      "LITERAL",
                      "ESCAPE",
                      "ESCAPED_LITERAL",
                      "ESCAPE",
                      "ESCAPED_LITERAL"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), expected);
        });

        it("should allow the escape symbol to escape itself", () => {

            const input = `^^`,
                  expected = [
                      "ESCAPE",
                      "ESCAPED_LITERAL"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), expected);
        });
    });

    describe("Variables", () => {
        /*
         * Variable expansion isn't handled by the lexer.  For that,
         * see the test file: 'var_expander_test.js'.
         */
    });
});
