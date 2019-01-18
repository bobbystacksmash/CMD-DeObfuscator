const assert   = require("chai").assert,
      tokenise = require("../../../lib/parser/tokeniser");

const util = {
    filterEOF: (tokens) => tokens.filter(t => t.name !== "EOF"),
    lexemes:   (tokens) => util.filterEOF(tokens).map(t => t.text),
    names:     (tokens) => util.filterEOF(tokens).map(t => t.name)
};

describe("Tokeniser", () => {

    describe("Escaping", () => {

        it("should identify an escape (^) and its associated escaped literal", () => {

            assert.deepEqual(
                util.names(tokenise(`^a`)), ["ESCAPE", "ESCAPED_LITERAL"]
            );
        });

        it("should escape any special meaning assigned to commands", () => {

            const tests = [
                {
                    input: `^"abc"def"`,
                    output: [
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "LITERAL",
                        "LITERAL",
                        "LITERAL",
                        "STRING_DQUOTE"
                    ]
                },
                {
                    input: `^%foo^%`,
                    output: [
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "LITERAL",
                        "LITERAL",
                        "LITERAL",
                        "ESCAPE",
                        "ESCAPED_LITERAL"
                    ]
                },
                //
                // Delimiters
                //
                // Special handling for LF:
                {
                    input: `^\n`,
                    output: ["ESCAPE", "LITERAL"]
                },
                {
                    input: `^ `,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^ `,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^;`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^=`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^\xff`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^\x0b`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^\x0c`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: `^\xff`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Parenthesis
                //
                {
                    input: "^( ^)",
                    output: [
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "DELIMITER",
                        "ESCAPE",
                        "ESCAPED_LITERAL"
                    ]
                },
                //
                // Brackets
                //
                {
                    input: `^[^]^{^}`,
                    output: [
                        "ESCAPE", "ESCAPED_LITERAL",
                        "ESCAPE", "ESCAPED_LITERAL",
                        "ESCAPE", "ESCAPED_LITERAL",
                        "ESCAPE", "ESCAPED_LITERAL"
                    ]
                },
                //
                // Escape an escape
                //
                {
                    input: `^^`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Escape an exclaimation mark
                //
                {
                    input: `^!`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Single quotes
                //
                {
                    input: `^'`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Plus (+)
                //
                {
                    input: `^+`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Commas
                //
                {
                    input: `^,`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Backtick
                //
                {
                    input: "^`",
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Tilde
                //
                {
                    input: `^~`,
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                //
                // Conditional Processing (&, &&, ||)
                //
                {
                    input: "^&",
                    output: ["ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: "^&^&",
                    output: ["ESCAPE", "ESCAPED_LITERAL", "ESCAPE", "ESCAPED_LITERAL"]
                },
                {
                    input: "^|^|",
                    output: ["ESCAPE", "ESCAPED_LITERAL", "ESCAPE", "ESCAPED_LITERAL"]
                }
            ];

            tests.forEach(t => {
                assert.deepEqual(util.names(tokenise(t.input)), t.output);
            });
        });

        it("should not be possible to escape a closing double quote", () => {

            const tests = [
                {
                    input: `^",^"a"`,
                    output: [
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "DELIMITER",
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "LITERAL",
                        "STRING_DQUOTE_START"
                    ]
                },
                {
                    input: `^"a"`,
                    output: [
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "LITERAL",
                        "STRING_DQUOTE_START"
                    ]
                },
                {
                    lexemes: true,
                    input:   `"a^"`,
                    output: [
                        "STRING_DQUOTE"
                    ]
                }
            ];

            tests.forEach(t => {

                assert.deepEqual(util.names(tokenise(t.input)), t.output);

                if (t.hasOwnProperty("lexemes")) {
                    // Input should match output
                    assert.deepEqual(util.lexemes(tokenise(t.input)), [t.input]);
                }
            });
        });
    });

    describe("String Identification", () => {

        describe("Double Quoted Strings", () => {

            it("should successfully identify double quoted strings", () => {

                const tests = [
                    {
                        input: `"abc"`,
                        lexemes: [`"abc"`],
                        names:   ["STRING_DQUOTE"],
                        msg:     "Identify a double quoted string"
                    },
                    {
                        input:   `"''"`,
                        lexemes: [`"''"`],
                        names:   ["STRING_DQUOTE"],
                        msg:     "Not get confused by single quotes within DQS."
                    },
                    {
                        input:   `"foo\\"`,
                        lexemes: [`"foo\\"`],
                        names:   ["STRING_DQUOTE"],
                        msg:     "Not possible to escape a dquote."
                    }
                ];

                tests.forEach(t => {
                    const tokens  = tokenise(t.input),
                          names   = util.names(tokens);

                    assert.deepEqual(util.lexemes(tokens), t.lexemes, t.msg);
                    assert.deepEqual(util.names(tokens),   t.names, t.msg);
                });
            });

            it("should identify a lone dquote as a literal", () => {
                const tokens = tokenise(`"`);
                assert.deepEqual(util.names(tokens), ["STRING_DQUOTE_START"]);
            });

            it("should identify an unterminated string", () => {
                const tokens = tokenise(`"ab`);
                assert.deepEqual(
                    util.names(tokens),
                    ["STRING_DQUOTE_START", "STRING_DQUOTE_CHAR"]
                );

                assert.deepEqual(util.lexemes(tokens), [`"`, "ab"]);
            });
        });
    });

    describe("Chars -> Units", () => {

        // A unit is just a string of characters that represents some
        // part of the command string.

        it("should identify the following characters as delimiters", () => {

            const tests = [
                { char: `,`,    name: "comma"              },
                { char: `;`,    name: "semi-colon"         },
                { char: `=`,    name: "equals"             },
                { char: ` `,    name: "space"              },
                { char: `\t`,   name: "tab"                },
                { char: `\n`,   name: "newline"            },
                { char: `\xff`, name: "strend"             },
                { char: `\x0b`, name: "vertical tab"       },
                { char: `\x0c`, name: "new page form feed" }
            ];

            tests.forEach(t => {
                assert.deepEqual(
                    util.names(tokenise(t.char)),
                    ["DELIMITER"],
                    `Delimiter ${t.name} detected`
                );
            });
        });

        it("should identify a continuous run of delimiters as a single unit", () => {

            const input    = ",;= \t;; \t\xff\x0b\x0c  ",
                  output   = tokenise(input);

            assert.isArray(output);
            assert.equal(output.length, 1);
            assert.equal(output[0].name, "DELIMITER");
            assert.equal(output[0].text, input);
        });

        it("should identify a runs of alternating delimiter/literal units", () => {

            const input  = `,;,cmd,;,`,
                  output = tokenise(input),
                  expected = [
                      { name: "DELIMITER", lexeme: ",;," },
                      { name: "LITERAL",   lexeme: "c"   },
                      { name: "LITERAL",   lexeme: "m"   },
                      { name: "LITERAL",   lexeme: "d"   },
                      { name: "DELIMITER", lexeme: ",;," },
                  ];

            assert.deepEqual(util.names(output),   expected.map(x => x.name));
            assert.deepEqual(util.lexemes(output), expected.map(x => x.lexeme));
        });
    });

    describe("Parenthesis", () => {

        const input = `(cmd)`,
              output = tokenise(input);

        assert.deepEqual(
            util.names(output),
            ["LPAREN", "LITERAL", "LITERAL", "LITERAL", "RPAREN"]
        );

        it("should detect nested parens", () => {

            const input = `((()))`,
                  output = tokenise(input);

            assert.deepEqual(
                util.names(output),
                [
                    "LPAREN",
                    "LPAREN",
                    "LPAREN",
                    "RPAREN",
                    "RPAREN",
                    "RPAREN"
                ]
            );
        });
    });

    describe("Conditional Processing '&', '&&', '||'", () => {

        it("should identify all of the grouping operators", () => {

            const tests = [
                { input: "&", name: "COND_ALWAYS" },
                { input: "&&", name: "COND_SUCCESS" },
                { input: "||", name: "COND_OR" }
            ];

            tests.forEach(t => assert.deepEqual(util.names(tokenise(t.input)), [t.name]));
        });

        it("should identify grouping operators when used in a command with other tokens", () => {

            const tests = [
                {
                    input: "a & b",
                    output: [
                        "LITERAL",
                        "DELIMITER",
                        "COND_ALWAYS",
                        "DELIMITER",
                        "LITERAL"
                    ]
                },
                {
                    input: "a && b",
                    output: [
                        "LITERAL",
                        "DELIMITER",
                        "COND_SUCCESS",
                        "DELIMITER",
                        "LITERAL"
                    ]
                },
                {
                    input: "(a || b)",
                    output: [
                        "LPAREN",
                        "LITERAL",
                        "DELIMITER",
                        "COND_OR",
                        "DELIMITER",
                        "LITERAL",
                        "RPAREN"
                    ]
                }
            ];

            tests.forEach(t => {
                assert.deepEqual(util.names(tokenise(t.input)), t.output);
            });

        });
    });

    describe("IF statements", () => {

        describe("File Syntax", () => {

            it("should tokenise an input string matching a simple IF EXIST", () => {

                const input  = `IF EXIST "abc.txt" CALL calc`,
                      output = [
                          "IF",
                          "DELIMITER",
                          "EXIST",
                          "DELIMITER",
                          "STRING_DQUOTE",
                          "DELIMITER",
                          "CALL",
                          "DELIMITER",
                          "LITERAL",
                          "LITERAL",
                          "LITERAL",
                          "LITERAL"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });
        });

        describe("String Syntax", () => {

        });

        describe("Error Check Syntax", () => {

        });

        it("should detect an 'IF DEFINED _var' sequence", () => {

            const input  = "IF DEFINED _x CALL calc",
                  output = [
                      "IF",
                      "DELIMITER",
                      "DEFINED",
                      "DELIMITER",
                      "LITERAL",
                      "LITERAL",
                      "DELIMITER",
                      "CALL",
                      "DELIMITER",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL",
                      "LITERAL"
                  ];

            assert.deepEqual(util.names(tokenise(input)), output);
        });
    });

    describe("Redirection and Pipes", () => {

        it("should identify <, >, >>, and |", () => {

            const tests = [
                { input: `<`,  output: "REDIRECT_IN"            },
                { input: `>`,  output: "REDIRECT_OUT"           },
                { input: `2>`, output: "REDIRECT_OUT_TO"        },
                { input: `&1`, output: "REDIRECT_STDERR_STDOUT" },
                { input: `>>`, output: "REDIRECT_OUT_APPEND"    },
                { input: `|`,  output: "REDIRECT_PIPE"          }
            ];

            tests.forEach(
                t => assert.deepEqual(util.names(tokenise(t.input)), [t.output])
            );
        });
    });

    describe("Delayed Expansion", () => {

        // A note about the use of '!' in command line mode
        // ================================================
        //
        // In batch mode, it seems that the presence of a '!' anywhere
        // on the line when delayed expansion is enabled causes the
        // '!' to be interpreted as a special character.  This does
        // not appear to be the case when entered on the command line.
        // It behaves much like the '%' does, where if it encloses a
        // variable and the value exists that value is replaced,
        // however is the variable is not defined the tokens are
        // copied to the output stream.
        //

    });
});
