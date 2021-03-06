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

    describe("Whitespace clean-ups", () => {

        it("should strip contiguous whitespace literals", () => {

            const inputs = [
                [ `cmd`,       `cmd`  ],
                [ `c m d`,     `c m d`],
                [ `c  m  d`,   `c m d`],
                [ `c   m d`,   `c m d`],
                [ `  c m d`,   `c m d`],
                [ `  c m d  `, `c m d`],
            ];

            inputs.forEach(test => {

                let tokens = CMD.tokenise(test[0], { filter: false });

                assert.deepEqual(
                    util.tokens2string(CMD.filter.strip_whitespace(tokens)),
                    test[1]
                );
            });
        });
    });

    describe("Strip excessive commas", () => {

        it("should remove excessive commas outside of double quotes", () => {

            const tests = [
                { input: "cmd , , ,", output: "cmd   " }
            ];

            tests.forEach(test => {
                assert.deepEqual(
                    util.tokens2string(CMD.filter.strip_commas(CMD.tokenise(test.input))),
                    test.output
                );
            });
        });
    });

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

        it("should handle multiple quoted regions in a single region", () => {

            const input  = CMD.tokenise(`h"t"t"p"`),
                  output = `"http"`;

            assert.deepEqual(util.tokens2string(CMD.filter.widen_strings(input)), output);
        });

        it("should widen a run of non-whitespace delimited chars in to one string", () => {

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

    describe("Combinations of string obfuscation", () => {

        it("should sanitise mixing quoted chars and empty chars in one string", () => {

            const tests = [
                [`w""sc"r"i"p"t`, [`"wscript"`]]
            ];

            tests.forEach(test => assert.deepEqual(CMD.parse(test[0]), test[1]));
        });
    });
});
