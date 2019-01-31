const assert           = require("chai").assert,
      tokenise         = require("../../lib/tokeniser"),
      interpret        = require("../../lib/interpreter").interpreter,
      identify_command = require("../../lib/interpreter").try_identify_command,
      split_cmd        = require("../../lib/interpreter").get_cached_cmd_blocks;

const util = {
    filterEOF: (tokens) => tokens.filter(t => t.name !== "EOF"),
    lexemes:   (tokens) => util.filterEOF(tokens).map(t => t.text),
    names:     (tokens) => util.filterEOF(tokens).map(t => t.name)
};

describe("Interpreter", () => {

    describe("Variable Expansion", () => {

        it("should expand a lone '%comspec%' correctly", () => {

            const input  = `%comspec%`,
                  output = [{
                      vars: { thisframe: {}, nextframe: {} },
                      commands: [
                          {
                              command: { name: "cmd", line: "" },
                              options: {}
                          }
                      ]
                  }];

            assert.deepEqual(interpret(input), output);
        });

        it("should expand '%comspec%' and parse its arguments as if it were 'cmd.exe'", () => {

            const input  = `%comspec% /c "calc"`,
                  output = [
                      {
                          vars: { thisframe: {}, nextframe: {} },
                          commands: [
                              {
                                  command: { name: "cmd", line: `"calc"` },
                                  options: { run_then_terminate: true }
                              },
                              {
                                  command: { name: "calc", line: "" },
                                  options: {}
                              }
                          ]
                      }
                  ];

            const result = interpret(input);
            assert.deepEqual(result, output);
        });

        it("should create a context correctly for an expanded %comspec% var", () => {

            const input  = `echo %comspec%`,
                  output = [
                      {
                          vars: { thisframe: {}, nextframe: {} },
                          commands: [
                              {
                                  command: { name: "echo", line: "C:\\Windows\\System32\\cmd.exe" },
                                  options: {}
                              }
                          ]
                      }
                  ];
            assert.deepEqual(interpret(input), output);
        });
    });

    describe("De-obfuscation", () => {

        describe("Split in to command blocks", () => {

            it("should create a single block when no other commands exist", () => {
                const input  = `calc`,
                      output = [
                          [{ name: "LITERAL", text: "calc" }]
                      ];

                assert.deepEqual(split_cmd(tokenise(input)), output);
            });

            it("should split a command in to blocks", () => {

                const input  = `calc && (powershell) || wscript & notepad`,
                      output = [
                          ["calc", " "],
                          [" ", "(", "powershell", ")", " "],
                          [" ", "wscript", " "],
                          [" ", "notepad"]
                      ];

                let result = split_cmd(tokenise(input));
                assert.deepEqual(split_cmd(tokenise(input)).map(res => res.map(r => r.text)), output);
            });
        });

        describe("Escapes", () => {

            it("should return the input without escapes", () => {
                const input  = `^p^o^w^e^r^s^h^e^l^l`,
                      output = [
                          {
                              vars: { nextframe: {}, thisframe: {} },
                              commands: [
                                  {
                                      command: { name: "powershell", line: "" },
                                      options: {}
                                  }
                              ]
                          }
                      ];
                assert.deepEqual(interpret(input), output);
            });

            it("should disable escapes when 'strip_escapes' is false", () => {
                const input  = `^p^o^w^e^r^s^h^e^l^l`,
                      output = [{
                          vars: { nextframe: {}, thisframe: {} },
                          commands: [{
                              command: { name: input, line: "" },
                              options: {}
                          }]
                      }];

                assert.deepEqual(interpret(input, { strip_escapes: false }), output);
            });
        });

        describe("Strings", () => {

            xit("should concatenate runs of regular strings and literals without delims", () => {
                const input  = `"abc"def"ghi"`,
                      output = [`"abcdefghi"`];
                assert.deepEqual(interpret(input), output);
            });

            it("should strip empty strings", () => {
                const input  = `w""scr""ipt""`,
                      output = [{
                          vars: { thisframe: {}, nextframe: {} },
                          commands: [{
                              command: { name: "wscript", line: "" },
                              options: {}
                          }]
                      }];

                assert.deepEqual(interpret(input), output);
            });

            xit("should unify strings or literals not separated by a delimiter", () => {

                const tests = [
                    {
                        input:  `w"s"cri"pt"`,
                        output: [`"wscript"`]
                    },
                    {
                        input:  `""cscr"ipt"`,
                        output: [`"cscript"`]
                    },
                    {
                        input:  `"a"b`,
                        output: [`"ab"`]
                    },
                    {
                        input: `ab"c"`,
                        output: [`abc`]
                    },
                    {
                        input: `abc /x:"f"oo`,
                        output: [`abc /x:"foo"`]
                    }
                ];

                tests.forEach(t => assert.deepEqual(interpret(t.input), t.output));
            });
        });
    });

    describe("Identifiying commands", () => {

        it("should correctly identify commands", () => {

            const tests = [
                {
                    input: `"C:\\Windows\\System32\\cmd.exe"`,
                    output: "cmd",
                    msg: "Read a quoted path without spaces, return exe."
                },
                {
                    input: `C:\\Windows\\System32\\cmd.exe`,
                    output: "cmd",
                    msg: "Read an unquoted path and return the exe."
                },
                {
                    input: `"C:\\Users\\Joe Bloggs\\Desktop\\calc.exe"`,
                    output: `calc`,
                    msg: "Read a quoted path containing a space and return the exe."
                },
                {
                    input: `..\\..\\Windows\\regsvr32.exe`,
                    output: `regsvr32`,
                    msg: "Read a relative path and return the exe."
                },
                {
                    input: `/foo/bar.exe`,
                    output: `bar`,
                    msg: "Read a relative path and return the exe."
                },
                {
                    input: `foo/bar.exe`,
                    output: `bar`,
                    msg: "Read a relative path and return the exe."
                },
                {
                    input: `calc.exe`,
                    output: `calc`,
                    msg: "Figure out that it's an exe and return the name."
                },
                {
                    input: `regsvr32.exe /s /n foo "bar"`,
                    output: `regsvr32`
                },
                {
                    input: ` regsvr32 a b c`,
                    output: `regsvr32`,
                    msg:    "Strip leading delimiters from cmdstr"
                },
                {
                    input: `((calc.exe))`,
                    output: `calc`,
                    msg:    "Strip parens, identify the exe."
                },
                {
                    input: `"set x=y"`,
                    output: "set",
                    msg:    "Identify 'set' between quotes."
                },
                {
                    input: `calc&`,
                    output: "calc",
                    msg: "Identify a command with conditional operator at the end."
                }
            ];

            tests.forEach(
                t => assert.deepEqual(identify_command(tokenise(t.input)).command, t.output, t.msg)
            );
        });

        it("should strip trailing closing parens from a single command", () => {

            const input = `(((calc)))`,
                  output = [{
                      vars: { thisframe: {}, nextframe: {} },
                      commands: [{
                          command: { name: "calc", line: "" },
                          options: {}
                      }]
                  }];

            assert.deepEqual(interpret(input), output);

        });

        it("should trim any trailing delimiters immediately after the identified cmd", () => {

            const tests = [
                {
                    input:  `regsvr32 foo bar`,
                    output: ["LITERAL", "DELIMITER", "LITERAL"]
                },
                {
                    input:  `regsvr32     foo bar`,
                    output: ["LITERAL", "DELIMITER", "LITERAL"]
                },
                {
                    input:  `regsvr32,,, ;;;     foo bar`,
                    output: ["LITERAL", "DELIMITER", "LITERAL"]
                },
                {
                    input:  `regsvr32,foo bar`,
                    output: ["LITERAL", "DELIMITER", "LITERAL"]
                },
                {
                    input:  `regsvr32;foo bar`,
                    output: ["LITERAL", "DELIMITER", "LITERAL"]
                },
                {
                    input: `regsvr32`,
                    output: []
                }
            ];

            tests.forEach(t => {
                let ident = identify_command(tokenise(t.input)).rest.map(x => x.name);
                assert.deepEqual(ident, t.output);
            });
        });
    });

    describe("Running commands", () => {

        describe("Context building", () => {

            it("should create nested contexts when using nested cmd.exe instances", () => {

                const input   = `cmd /c "ca""lc"`,
                      output  = [
                          {
                              vars: { thisframe: {}, nextframe: {} },
                              commands: [
                                  {
                                      command: { name: "cmd", line: `"calc"` },
                                      options: { run_then_terminate: true }
                                  },
                                  {
                                      command: { name: "calc", line: "" },
                                      options: {}
                                  }
                              ]
                          }

                      ];

                let context = interpret(input);
                assert.deepEqual(context, output);
            });

            it("should introduce envvars with SET when SET is a sub-command", () => {

                const input  = `cmd /c "set x=y"`,
                      output = [
                          {
                              vars: { thisframe: {}, nextframe: { x: "y" } },
                              commands: [
                                  {
                                      command: { name: "cmd", line: `"set x=y"` },
                                      options: { run_then_terminate: true }
                                  },
                                  {
                                      command: { name: "set", line: "x=y" },
                                      options: {}
                                  }
                              ]
                          },
                      ];

                let context = interpret(input);
                assert.deepEqual(context, output);
            });

            it("should not expand variables in the same context as a SET", () => {

                const input  = `SET x=y&& echo %x%`,
                      output = [
                          {
                              vars: { thisframe: {}, nextframe: { x: "y" } },
                              commands: [
                                  {
                                      command: { name: "set", line: "x=y" },
                                      options: {}
                                  },
                                  {
                                      command: { name: "echo", line: "%x%" },
                                      options: {}
                                  }
                              ]
                          }
                      ];

                assert.deepEqual(interpret(input), output);
            });

            it("should expand variables when 'CALL' is used", () => {
                // TODO
                // ====
                // Get 'CALL' expanding environment variables.
                //
                const input  = `cmd /c "set foo=bar&&call echo %foo%"`,
                      output = [
                          {
                              vars: {
                                  nextframe: {},
                                  thisframe: { foo: "bar" }
                              },
                              commands: [
                                  {
                                      command: { name: "call", line: "echo %foo%" },
                                      options: {}
                                  },
                                  {
                                      command: { name: "echo", line: "bar" },
                                      options: {}
                                  },
                              ]
                          },
                          {
                              vars: {
                                  nextframe: { foo: "bar" },
                                  thisframe: {}
                              },
                              commands: [
                                  {
                                      command: { name: "cmd", line: `"set foo=bar&&call echo %foo%"` },
                                      options: { run_then_terminate: true }
                                  },
                                  {
                                      command: { name: "set", line: `foo=bar` },
                                      options: {}
                                  }
                              ]
                          }
                      ];

                assert.deepEqual(interpret(input), output);
            });
        });

        describe("Nested commands", () => {

            it("should identify nested commands and write them in to the context", () => {

                const tests = [
                    {
                        input: `cmd /c "set a=b&calc"`,
                        output: [
                            {
                                vars: { nextframe: { a: "b" }, thisframe: {} },
                                commands: [
                                    {
                                        command: { name: "cmd", line: `"set a=b&calc"` },
                                        options: { run_then_terminate: true }
                                    },
                                    {
                                        command: { name: "set", line: "a=b" },
                                        options: {}
                                    },
                                    {
                                        command: { name: "calc", line: "" },
                                        options: {}
                                    }
                                ]
                            }
                        ]
                    }
                ];

                tests.forEach(t => {
                    assert.deepEqual(interpret(t.input), t.output);
                });
            });
        });

        describe("Command-specific features", () => {

            describe("CMD", () => {

                it("should identify CMD when used with an upper-case '/C'", () => {

                    const input  = `cmd.exe /C calc`,
                          output = [{
                              vars: { thisframe: {}, nextframe: {} },
                              commands: [
                                  {
                                      command: { name: "cmd", line: "calc" },
                                      options: { run_then_terminate: true }
                                  },
                                  {
                                      command: { name: "calc", line: "" },
                                      options: {}
                                  }
                              ]
                          }];

                    assert.deepEqual(interpret(input), output);
                });

                it("should handle a lone 'cmd.exe' with some flags", () => {

                    const input  = "cmd /V",
                          output = [{
                              vars: { thisframe: {}, nextframe: {} },
                              commands: [{
                                  command: { name: "cmd", line: "" },
                                  options: { delayed_expansion: true }
                              }]
                          }];

                    assert.deepEqual(interpret(input), output);
                });

                it.only("should enable delayed expansion for all valid '/V' combinations", () => {

                    const tests = [
                        { cmd: `cmd /V`, flag: true },
                        { cmd: `cmd /V:O`, flag: true },
                        { cmd: `cmd /V:ON`, flag: true },
                        { cmd: `cmd /V:Off`, flag: false }
                    ];

                    const on_context = [{
                        vars: { thisframe: {}, nextframe: {} },
                        commands: [
                            {
                                command: { name: "cmd", line: "" },
                                options: { delayed_expansion: true }
                            }
                        ]
                    }];

                    const off_context = [{
                        vars: { thisframe: {}, nextframe: {} },
                        commands: [
                            {
                                command: { name: "cmd", line: "" },
                                options: { delayed_expansion: false }
                            }
                        ]
                    }];


                    tests.forEach(t => assert.deepEqual(interpret(t.cmd), (t.flag) ? on_context : off_context));
                });
            });
        });
    });
});
