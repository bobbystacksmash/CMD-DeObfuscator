const assert       = require("chai").assert,
      deobfuscator = require("../index");

describe("DeObfuscator Tests", () => {

    describe("Removing noise", () => {

        it("should remove empty double quotes", () => {

            const input  = `po""wershell`,
                  output = `powershell`;

            assert.equal(deobfuscator.deobfuscate(input), output);
        });
    });

    describe("Escapes", () => {

        it("should correctly handle escapes and double-escapes", () => {

        });
    });
});
