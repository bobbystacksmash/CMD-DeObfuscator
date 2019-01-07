const assert = require("chai").assert,
      CMD    = require("../../index");

const util = {
    tokens: function (tokens) {
        return tokens.map(token => token.name);
    },
    tokens2string: function (tokens) {
        return tokens.map(t => t.text).join("");
    }
};

describe("Command handlers", () => {

    describe("cmd.exe", () => {

        describe("Switches", () => {

            it("should correctly identify the various cmd.exe switches", () => {

                let tests = [
                    { input: `cmd.exe /V`, expected: { V: true } },
                    { input: `"C:\\Windows\\System32\\cmd.exe" /c c^m^d`, expected: { "c": "" } },
                    { input: `"cmd.exe" /R "set x=y"`, expected: { "R": "" } },
                ];

                tests.forEach(test => {

                    let tokens = CMD.tokenise(test.input),
                        ident  = CMD.try_identify_command(tokens),
                        output = CMD.filter.handle_CMD(ident, tokens);

                    assert.deepEqual(output.switches, test.expected);
                });
            });
        });

        it("should not alter the token sequence when the identified cmd has no args", () => {

            let tests = [
                `cmd.exe`,
                `"cmd"`,
                `cmd`,
                `wscript`,
                `p^o^w^e^r^s^h^e^l^l`
            ];

            tests.forEach(t => {

                let tokens = CMD.tokenise(t),
                    ident  = CMD.try_identify_command(tokens),
                    output = CMD.filter.handle_CMD(ident, tokens);

                assert.deepEqual(output, tokens);
            });
        });

        xit("should also strip command flags before the body of the command", () => {

            let tests = [
                {
                    input:  `"C:\\Windows\\System32\\cmd.exe" /c cmd "set foo=bar"`,
                    output: [`set foo=bar`]
                }
            ];

            tests.forEach(t => {
                assert.deepEqual(CMD.parse(t.input), t.output);
            });
        });
    });
});
