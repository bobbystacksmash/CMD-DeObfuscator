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

describe("String handling and clean-up filters", () => {

    describe("Strip Empty Strings", () => {

        it("should remove empty double-quotes from a given command string", () => {

            const inputs = [
                [ `""powershell`,           `powershell` ],
                [ `powershell""`,           `powershell` ],
                [ `""pow""ershell""`,       `powershell` ],
                [ `pow""ershell`,           `powershell` ],
                [ `p""o""w""e""r""s""hell`, `powershell` ]
            ];

            inputs.forEach(test => {
                assert.deepEqual(
                    util.tokens2string(CMD.filter.strip_empty_strings(CMD.tokenise(test[0]))),
                    test[1]
                );
            });
        });
    });

    describe("Widen Strings", () => {

        it("should widen a contiguous run of non-whitespace delimited chars in to one string", () => {

            const input  = CMD.tokenise(`echo c"al"c.exe test`),
                  output = `echo "calc.exe" test`;

            assert.deepEqual(util.tokens2string(CMD.filter.widen_strings(input)), output);
        });

        it("should not alter strings already at whitespace bounaries", () => {

            const input  = CMD.tokenise(`echo "calc.exe"`),
                  output = `echo "calc.exe"`;

            assert.deepEqual(util.tokens2string(CMD.filter.widen_strings(input)), output);
        });
    });
});
