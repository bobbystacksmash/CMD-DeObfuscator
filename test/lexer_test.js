const assert       = require("chai").assert,
      CMD = require("../index");

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

describe("CMD Tests", () => {

    describe("Filtering tokens", () => {

        it("should, by default, remove excessive whitespace outside of strings", () => {

            const input    = `cmd     a  b   c`,
                  expected = [
                      { name: "LITERAL", text: "c" },
                      { name: "LITERAL", text: "m" },
                      { name: "LITERAL", text: "d" },
                      { name: "LITERAL", text: " " },
                      { name: "LITERAL", text: "a" },
                      { name: "LITERAL", text: " " },
                      { name: "LITERAL", text: "b" },
                      { name: "LITERAL", text: " " },
                      { name: "LITERAL", text: "c" }
                  ];

            const tokens = CMD.tokenise(input).map(token => {
                return { name: token.name, text: token.text };
            });

            assert.deepEqual(tokens, expected);
        });

        it("should, by default, remove empty double-quotes from a command", () => {

            const input  = `w""script`,
                  output = [
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });

        it(`should filter strings like: '"h"t"t"p'`, () => {

            const input  = `"h"t"t"p`,
                  unfiltered = [
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_END",
                      "LITERAL",
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_END",
                      "LITERAL"
                  ];

            const filtered = [
                "STRING_DQUOTE_BEGIN",
                "STRING_DQUOTE_CHAR",
                "STRING_DQUOTE_CHAR",
                "STRING_DQUOTE_CHAR",
                "STRING_DQUOTE_CHAR",
                "STRING_DQUOTE_END"
            ];

            assert.deepEqual(
                util.tokens(CMD.tokenise(input, { filter: false })),
                unfiltered,
                "disable filtering"
            );

            assert.deepEqual(
                util.tokens(CMD.tokenise(input, { filter: true })),
                filtered,
                "filtering"
            );
        });
    });

    describe("using SET to introduce environment variables", () => {

        it(`should detect a 'SET' command`, () => {

            const input  = `set `,
                  output = ["SET"];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });

        it(`should correctly tokenise the statement: SET "^ =10"`, () => {

            const input  = `set "^ =10"`,
                  output = [
                      "SET",
                      "SET_DQUOTE_BEGIN",
                      "SET_DQUOTE_CHAR",
                      "SET_DQUOTE_CHAR",
                      "SET_ASSIGNMENT",
                      "SET_DQUOTE_CHAR",
                      "SET_DQUOTE_CHAR",
                      "SET_DQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });

        it(`should correctly tokenise the statement: SET foo=bar`, () => {

            const input  = `set foo=bar`,
                  output = [
                      "SET",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "SET_ASSIGNMENT",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });

        it(`should treat a space as a valid part of a var name`, () => {

            const input  = `SET _x =ab`,
                  output = [
                      "SET",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "SET_ASSIGNMENT",
                      "LITERAL",
                      "LITERAL"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });

        it("should allow an escaped caret to be a valid LHS name", () => {

            const input    = `SET ^^=abc`,
                  expected = [
                      ["SET", "SET "],
                      ["LITERAL", "^"],
                      ["SET_ASSIGNMENT", "="],
                      ["LITERAL", "a"],
                      ["LITERAL", "b"],
                      ["LITERAL", "c"]
                  ];

            let tokens = CMD.tokenise(input).map(t => [t.name, t.text]);
            assert.deepEqual(tokens, expected);
        });

        it(`should continue RHS assignment until meeting an '&' (CALL)`, () => {

            const input  = `SET foo=a,b,c &`,
                  output = [
                      "SET",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "SET_ASSIGNMENT",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "CALL"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });

        it("should tokenise an obfuscated SET command in to a clean SET command", () => {

            const input  = `^S^ET foo=bar`,
                  output = [
                      "SET",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "SET_ASSIGNMENT",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
        });
    });

    describe("Quotes", () => {

        describe("Double-Quoted strings (DQS)", () => {

            it("should correctly detect an empty string", () => {

                const input = `""`,
                      output = [
                          "STRING_DQUOTE_BEGIN",
                          "STRING_DQUOTE_END"
                      ];

                assert.deepEqual(
                    util.tokens(CMD.tokenise(input, { filter: false })),
                    output
                );
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

                assert.deepEqual(
                    util.tokens(CMD.tokenise(input, { filter: false })),
                    output
                );
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

                assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
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

                assert.deepEqual(
                    util.tokens(CMD.tokenise(input, { filter: false })),
                    output
                );
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

                assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
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

            assert.deepEqual(util.tokens(CMD.tokenise(input, { filter: false })), output);
        });

        it("should allow the escape symbol to escape itself", () => {

            const input = `^^`,
                  output = [
                      "ESCAPE",
                      "ESCAPED_LITERAL"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input, { filter: false })), output);
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

            assert.deepEqual(util.tokens(CMD.tokenise(input, { filter: false })), output);
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

            assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
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

            assert.deepEqual(util.tokens(CMD.tokenise(input, { filter: false })), output);
        });

        describe("Filtered tokens", () => {

            it("should change all escaped pairs in to a literal char", () => {

                const input  = `f^o^o`,
                      output = [
                          "LITERAL",
                          "LITERAL",
                          "LITERAL"
                      ];

                assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
            });

            it("should correctly deobfuscate and identify 'SET'", () => {

                const input  = "S^E^T foo=bar",
                      output = [
                          "SET",
                          "LITERAL",
                          "LITERAL",
                          "LITERAL",
                          "SET_ASSIGNMENT",
                          "LITERAL",
                          "LITERAL",
                          "LITERAL"
                      ];

                assert.deepEqual(util.tokens(CMD.tokenise(input)), output);
            });

            it("should handle escaped escape chars", () => {

                let tokenised = CMD.tokenise(`^^`);

                assert.isArray(tokenised);
                assert.equal(tokenised.length, 1);

                assert.equal(tokenised[0].name, "LITERAL");
                assert.equal(tokenised[0].text, "^");
            });
        });
    });

    describe("Commas and Semi-colons", () => {

        // A free comma is a comma that appears outside of quotes.
        it("should correctly identify free-commas", () => {

            const input  = `,;,`,
                  output = [
                      "COMMA",
                      "SEMICOLON",
                      "COMMA"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input, { filter: false })), output);
        });

        it("should not detect commas and semi-colons between double quotes", () => {

            const input  = `,;,",;,"`,
                  output = [
                      "COMMA",
                      "SEMICOLON",
                      "COMMA",
                      "STRING_DQUOTE_BEGIN",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_CHAR",
                      "STRING_DQUOTE_END"
                  ];

            assert.deepEqual(util.tokens(CMD.tokenise(input, { filter: false })), output);
        });

    });

    describe("Variables", () => {
        /*
         * Variable expansion isn't handled by the lexer.  For that,
         * see the test file: 'var_expander_test.js'.
         */
    });
});
