const assert = require("chai").assert,
      DOS    = require("../index");

describe("Deobfuscation Tests", () => {

    describe("Removing noise", () => {

        it("should remove empty double quotes", () => {

            const input  = `po""wershell`,
                  output = `powershell`;

            assert.equal(DOS.deobfuscate(input).command, output);
        });

        it(`should remove multiple SPACE chars in a row outside of a DQS`, () => {

            const input  = `cMd     /c    calc.exe`,
                  output = `cMd /c calc.exe`;

            assert.equal(DOS.deobfuscate(input).command, output);

            // Do not apply to DQS or SQS strings.
            const dqs = `cmd /c "f  o  o"`;
            assert.equal(DOS.deobfuscate(dqs).command, dqs);

            const sqs = `cmd /c 'f  o  o'`;
            assert.equal(DOS.deobfuscate(sqs).command, sqs);
        });

        it(`should remove escapes which preceed spaces`, () => {

            const input  = `dir ^ `,
                  output = `dir `;

            assert.equal(DOS.deobfuscate(input).command, output);
        });
    });

    describe("Environment variables", () => {

        it("should correctly identify variable assignments using SET", () => {

            const tests = [
                { in: `SET foo=bar`, out: { foo: "bar" } },
                { in: `SET foo = bar`, out: { "foo ": " bar" } }
            ];

            tests.forEach(T => assert.deepEqual(DOS.deobfuscate(T.in).vars, T.out));
        });

        it("should include double quotes as part of the var value", () => {

            const tests = [
                { in: `SET foo="bar"`, out: {foo: `"bar"` } }
            ];

            tests.forEach(T => assert.deepEqual(DOS.deobfuscate(T.in).vars, T.out));
        });

        it("should allow punctuation chars in the variable name", () => {

            const tests = [
                { in: `SET ###=bar`, out: { "###": "bar" } },
                { in: `SET  =bar`,   out: { " ": "bar" } },
            ];

            tests.forEach(T => assert.deepEqual(DOS.deobfuscate(T.in).vars, T.out));
        });

        // complain about !!!! in var names
    });

    /*xdescribe("Escapes", () => {

        it("should correctly handle escapes and double-escapes", () => {

        });
    });*/
});
