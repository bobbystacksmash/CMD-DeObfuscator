const assert   = require("chai").assert,
      tokenise = require("../../../lib/tokeniser");


describe("Tokeniser", () => {

    describe("Chars -> Units", () => {

        // A unit is just a string of characters that represents some
        // part of the command string.

        it("should identify a continuous run of delimiters as a single unit", () => {

            const input    = ",;= \t",
                  output   = tokenise(input);

            assert.isArray(output);
            assert.equal(output.length, 2);
            assert.equal(output[0].name, "DELIMITER");
            assert.equal(output[0].text, input);
        });
    });
});
