const assert       = require("chai").assert,
      deobfuscator = require("../index");

describe("DeObfuscator Tests", () => {

    describe("Removing noise", () => {

        it("should remove empty double quotes", () => {

            const input  = `po""wershell`,
                  output = `powershell`;

            assert.equal(deobfuscator.deobfuscate(input), output);
        });

        it(`should remove multiple SPACE chars in a row outside of a DQS`, () => {

            const input  = `cMd     /c    calc.exe`,
                  output = `cMd /c calc.exe`;

            assert.equal(deobfuscator.deobfuscate(input), output);

            // Do not apply to DQS or SQS strings.
            const dqs = `cmd /c "f  o  o"`;
            assert.equal(deobfuscator.deobfuscate(dqs), dqs);

            const sqs = `cmd /c 'f  o  o'`;
            assert.equal(deobfuscator.deobfuscate(sqs), sqs);
        });

        it(`should remove escapes which preceed spaces`, () => {

            const input  = `dir ^ `,
                  output = `dir `;

            assert.equal(deobfuscator.deobfuscate(input), output);
        });
    });

    describe("Escapes", () => {

        it("should correctly handle escapes and double-escapes", () => {

        });
    });
});
