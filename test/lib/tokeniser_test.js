const assert   = require("chai").assert,
      tokenise = require("../../lib/tokeniser");

const util = {
    filterEOF: (tokens) => tokens.filter(t => t.name !== "EOF"),
    lexemes:   (tokens) => util.filterEOF(tokens).map(t => t.text),
    names:     (tokens) => util.filterEOF(tokens).map(t => t.name)
};

describe("Tokeniser", () => {

    describe("Reserved Word List", () => {

        it("should detect words in the reserved word list", () => {

            const reserved = [
                "SET", "IF", "ELSE", "NOT", "DEFINED", "CALL"
            ];

            reserved.forEach(r => assert.deepEqual(util.names(tokenise(r)), [r]));
        });
    });

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
                        "STRING_DQUOTE"
                    ]
                },
                {
                    input: `^%foo^%`,
                    output: [
                        "ESCAPE",
                        "ESCAPED_LITERAL",
                        "LITERAL",
                        "ESCAPE",
                        "ESCAPED_LITERAL"
                    ]
                },
                //
                // Delimiters
                //
                // Special handling for LF:
                //
                // TODO: do not think LF loses any special meaning...
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

    describe.only("SET", () => {

        it("should detect a SET= where the RHS of = contains delimiters", () => {

            const input  = `SET abc=x;y;z;&&echo`,
                  output = [
                      "SET",
                      "DELIMITER",
                      "SET_LITERAL",
                      "SET_ASSIGN",
                      "SET_VALUE",
                      "COND_SUCCESS",
                      "LITERAL"
                  ];

            assert.deepEqual(util.names(tokenise(input)), output);
        });

        it("should detect a standard SET statement", () => {

            const input  = `SET abc=def`,
                  output = [
                      "SET",
                      "DELIMITER",
                      "SET_LITERAL",
                      "SET_ASSIGN",
                      "SET_VALUE"
                  ];

            assert.deepEqual(util.names(tokenise(input)), output);
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
                      { name: "LITERAL",   lexeme: "cmd" },
                      { name: "DELIMITER", lexeme: ",;," },
                  ];

            assert.deepEqual(util.names(output),   expected.map(x => x.name));
            assert.deepEqual(util.lexemes(output), expected.map(x => x.lexeme));
        });
    });

    describe("Parenthesis", () => {

        it("should detect '(cmd)'", () => {

            const input = `(cmd)`,
                  output = tokenise(input);

            assert.deepEqual(
                util.names(output),
                ["LPAREN", "LITERAL", "RPAREN"]
            );
        });

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

                const tests = [
                    {
                        input: `IF EXIST "abc.txt" CALL calc`,
                        output: [
                            "IF",
                            "DELIMITER",
                            "EXIST",
                            "DELIMITER",
                            "STRING_DQUOTE",
                            "DELIMITER",
                            "CALL",
                            "DELIMITER",
                            "LITERAL",
                        ]
                    },
                    {
                        input: `IF EXIST abc.txt CALL calc`,
                        output: [
                            "IF",
                            "DELIMITER",
                            "EXIST",
                            "DELIMITER",
                            "LITERAL",
                            "DELIMITER",
                            "CALL",
                            "DELIMITER",
                            "LITERAL",
                        ]
                    }
                ];

                tests.forEach(t => {
                    assert.deepEqual(util.names(tokenise(t.input)), t.output, t.input);
                });
            });

            it("should tokenise an input string matching a simple 'IF NOT EXIST...'", () => {

                const tests = [
                    {
                        input: `IF NOT EXIST "abc.txt" CALL calc`,
                        output: [
                            "IF",
                            "DELIMITER",
                            "NOT",
                            "DELIMITER",
                            "EXIST",
                            "DELIMITER",
                            "STRING_DQUOTE",
                            "DELIMITER",
                            "CALL",
                            "DELIMITER",
                            "LITERAL",
                        ]
                    },
                    {
                        input: `IF NOT EXIST abc.txt CALL calc`,
                        output: [
                            "IF",
                            "DELIMITER",
                            "NOT",
                            "DELIMITER",
                            "EXIST",
                            "DELIMITER",
                            "LITERAL",
                            "DELIMITER",
                            "CALL",
                            "DELIMITER",
                            "LITERAL",
                        ]
                    }
                ];

                tests.forEach(t => {
                    assert.deepEqual(util.names(tokenise(t.input)), t.output, t.input);
                });
            });

            it("should tokenise an 'IF EXIST filename (cmd) ELSE (cmd)' expr", () => {

                const tests = [
                    {
                        input: `IF EXIST "abc.txt" (CALL calc.exe) ELSE (call wscript.exe)`,
                        output: [
                            "IF",
                            "DELIMITER",
                            "EXIST",
                            "DELIMITER",
                            "STRING_DQUOTE",
                            "DELIMITER",
                            "LPAREN",
                            "CALL",
                            "DELIMITER",
                            "LITERAL",
                            "RPAREN",
                            "DELIMITER",
                            "ELSE",
                            "DELIMITER",
                            "LPAREN",
                            "CALL",
                            "DELIMITER",
                            "LITERAL",
                            "RPAREN"
                        ]
                    }
                ];

                tests.forEach(t => {
                    assert.deepEqual(util.names(tokenise(t.input)), t.output, t.input);
                });
            });
        });

        describe("String Syntax", () => {

            it("should detect basic string comparison", () => {

                const input  = `IF "%var%"=="Hello, World!" ( ECHO found! )`,
                      output = [
                          "IF",
                          "DELIMITER",
                          "STRING_DQUOTE",
                          "DOUBLE_EQUALS",
                          "STRING_DQUOTE",
                          "DELIMITER",
                          "LPAREN",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "RPAREN"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });

            it("should detect empty strings when checking if a var is set", () => {

                const input  = `IF "%var%"=="" (SET var=123)`,
                      output = [
                          "IF",
                          "DELIMITER",
                          "STRING_DQUOTE",
                          "DOUBLE_EQUALS",
                          "STRING_DQUOTE",
                          "DELIMITER",
                          "LPAREN",
                          "SET",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL",
                          "RPAREN"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });

            it("should detect all of: EQU, NEQ, LSS, LEQ, GTR, GEQ", () => {

                const comparison_opterators = [
                    "EQU", "NEQ", "LSS", "LEQ", "GTR", "GEQ"
                ];

                comparison_opterators.forEach(op => {

                    const input  = `IF "a" ${op} "b" (calc.exe)`,
                          output = util.names(tokenise(input));

                    assert.deepEqual(output, [
                        "IF",
                        "DELIMITER",
                        "STRING_DQUOTE",
                        "DELIMITER",
                        `IF_${op}`,
                        "DELIMITER",
                        "STRING_DQUOTE",
                        "DELIMITER",
                        "LPAREN",
                        "LITERAL",
                        "RPAREN"
                    ]);
                });
            });

            it("should detect an argument between IF and the var", () => {

                const input  = `IF /A "%var%"=="abc" (call calc)`,
                      output = [
                          "IF",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "STRING_DQUOTE",
                          "DOUBLE_EQUALS",
                          "STRING_DQUOTE",
                          "DELIMITER",
                          "LPAREN",
                          "CALL",
                          "DELIMITER",
                          "LITERAL",
                          "RPAREN"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });

            // TODO:
            //  - IF "%var%"=="" (SET var=default value)
            //  - IF NOT DEFINED var (SET var=default value)
            // SET /A var=1
            // IF /I "%var%" EQU "1" ECHO equality with 1
            // IF /I "%var%" NEQ "0" ECHO inequality with 0
            // IF /I "%var%" GEQ "1" ECHO greater than or equal to 1
            // IF /I "%var%" LEQ "1" ECHO less than or equal to 1
            //
            // Illegal:
            // IF/I "%var%" EQU "1" ECHO equality with 1
            // IF /I"%var%" EQU "1" ECHO equality with 1
            // IF/I"%var%" EQU "1" ECHO equality with 1
        });

        it("should detect an 'IF DEFINED _var' sequence", () => {

            const input  = "IF DEFINED _x CALL calc",
                  output = [
                      "IF",
                      "DELIMITER",
                      "DEFINED",
                      "DELIMITER",
                      "LITERAL",
                      "DELIMITER",
                      "CALL",
                      "DELIMITER",
                      "LITERAL",
                  ];

            assert.deepEqual(util.names(tokenise(input)), output);
        });
    });

    describe.only("FOR loops", () => {
        //
        // See: https://ss64.com/nt/for.html.
        //
        describe("FOR-files", () => {

            // See: https://ss64.com/nt/for2.html
            it("should detect the basic FOR (files) loop", () => {

                const input  = `FOR %G IN (MyFile.txt) DO copy %G C:\\Windows`,
                      output = [
                          "FOR",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "IN",
                          "DELIMITER",
                          "LPAREN",
                          "LITERAL",
                          "RPAREN",
                          "DELIMITER",
                          "DO",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });

            it("should tokenise a FOR-files loop with a string paren section", () => {

                const input  = `FOR %G IN ("Hello World") DO Echo %G`,
                      output = [
                          "FOR",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "IN",
                          "DELIMITER",
                          "LPAREN",
                          "STRING_DQUOTE",
                          "RPAREN",
                          "DELIMITER",
                          "DO",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });
        });

        describe("FOR /R - loop through files", () => {
            //
            // See: https://ss64.com/nt/for_r.html
            //
            it("should detect a for /R loop", () => {
                const input  = `For /R C:\\temp\\ %G IN (*.bak) do Echo del "%G"`,
                      output = [
                          "FOR",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "IN",
                          "DELIMITER",
                          "LPAREN",
                          "LITERAL",
                          "RPAREN",
                          "DELIMITER",
                          "DO",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "LITERAL",
                          "DELIMITER",
                          "STRING_DQUOTE"
                      ];

                assert.deepEqual(util.names(tokenise(input)), output);
            });
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
