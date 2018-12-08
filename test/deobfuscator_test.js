const assert       = require("chai").assert,
      deobfuscator = require("../index");

describe("DeObfuscator Tests", () => {

    describe("Quotes", () => {

        it(`should remove empty double-quote pairs: ""`, () => {

            const input  = `Pow""erSh""ell`,
                  output = `PowerShell`;

            assert.equal(deobfuscator.deobfuscate(input), output);
        });
    });
});
