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

        // test for the case where the command is simply 'wscript'.
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

        it("should re-tokenise tokens when the command is identified", () => {

            let tokens   = CMD.tokenise(`cmd.exe "set foo=bar"`),
                ident    = CMD.try_identify_command(tokens),
                output   = util.tokens(CMD.filter.handle_CMD(ident, tokens)),
                expected = [
                    "SET",
                    "LITERAL",
                    "LITERAL",
                    "LITERAL",
                    "SET_ASSIGNMENT",
                    "LITERAL",
                    "LITERAL",
                    "LITERAL"
                ];

            assert.deepEqual(output, expected);
        });
    });
});
