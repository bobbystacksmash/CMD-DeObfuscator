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

        describe("Double-Quoted strings (DQS)", () => {

            it("should correctly detect an empty string", () => {

                const input = `""`,
                      output = [
                          "STRING_DQUOTE_BEGIN",
                          "STRING_DQUOTE_END"
                      ];

                assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
            });

            it("should detect an empty string between literals", () => {

                const input = `po""wer`,
                      output = [
                          "LITERAL",             // p
                          "LITERAL",             // o
                          "STRING_DQUOTE_BEGIN", // ""
                          "STRING_DQUOTE_END",   // ""
                          "LITERAL",             // w
                          "LITERAL",             // e
                          "LITERAL"              // r
                      ];

                assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
            });

            it("should correctly detect balanced double-quotes ", () => {

                const input    = `"abc"`,
                      output = [
                          "STRING_DQUOTE_BEGIN",
                          "STRING_DQUOTE_CHAR",
                          "STRING_DQUOTE_CHAR",
                          "STRING_DQUOTE_CHAR",
                          "STRING_DQUOTE_END"
                      ];

                assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
            });

            it("should handle empty double-quotes within a string", () => {

                const input = `""a""b`,
                      output = [
                          "STRING_DQUOTE_BEGIN",
                          "STRING_DQUOTE_END",
                          "LITERAL",
                          "STRING_DQUOTE_BEGIN",
                          "STRING_DQUOTE_END",
                          "LITERAL"
                      ];

                assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
            });
        });

        describe("Single-Quoted strings (SQS)", () => {

            it("should handle single quoted strings", () => {

                const input  = `'abc'`,
                      output = [
                          "STRING_SQUOTE_BEGIN",
                          "STRING_SQUOTE_CHAR",
                          "STRING_SQUOTE_CHAR",
                          "STRING_SQUOTE_CHAR",
                          "STRING_SQUOTE_END"
                      ];

                assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
            });
        });
    });

    describe("Escapes", () => {

        it("should detect the escape symbol", () => {

            const input    = `a^b^c`,
                  output = [
                      "LITERAL",
                      "ESCAPE",
                      "ESCAPED_LITERAL",
                      "ESCAPE",
                      "ESCAPED_LITERAL"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
        });

        it("should allow the escape symbol to escape itself", () => {

            const input = `^^`,
                  output = [
                      "ESCAPE",
                      "ESCAPED_LITERAL"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
        });

        it("should detect the escape symbol when used before double quote", () => {

            const input    = `^""foo"`,
                  output = [
                      "ESCAPE",
                      "ESCAPED_LITERAL",
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
        });

        it("should not escape tokens within a double-quoted string", () => {

            const input  = `"^f^o^o"`,
                  output = [
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
        });

        it("should detect escape tokens within a single-quoted string", () => {

            const input  = `'f^o^o'`,
                  output = [
                      "STRING_SQUOTE_BEGIN",
                      "STRING_SQUOTE_CHAR",
                      "ESCAPE",
                      "ESCAPED_LITERAL",
                      "ESCAPE",
                      "ESCAPED_LITERAL",
                      "STRING_SQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(deobfuscator.tokenise(input)), output);
        });
    });

    describe("Variables", () => {
        /*
         * Variable expansion isn't handled by the lexer.  For that,
         * see the test file: 'var_expander_test.js'.
         */
    });
});
