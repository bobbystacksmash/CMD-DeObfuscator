const assert = require("chai").assert,
      DOS    = require("../index");

describe("Command Splitter", () => {

    describe("Single Ampersand (CALL)", () => {

        it("should return a single-element array when there is only one command", () => {

            const input  = "echo foo",
                  output = [input];

            assert.deepEqual(DOS.split_command(input), output);
        });

        it("should return a single-elemeny array when the command ends with a CALL but no other text", () => {

            const input  = "echo foo &",
                  output = ["echo foo"];

            assert.deepEqual(DOS.split_command(input), output);
        });

        it("should remove empty commands", () => {

            const input  = "echo foo & & & calc.exe",
                  output = ["echo foo", "calc.exe"];

            assert.deepEqual(DOS.split_command(input), output);
        });
    });

    describe("Semi-Colon (SEMICOLON)", () => {

        it("should split a command separated by semi-colons", () => {

            const tests = [
                { input: "calc.exe ; notepad.exe", output: ["calc.exe", "notepad.exe"] }
            ];

            tests.forEach(t => {
                assert.deepEqual(DOS.split_command(t.input), t.output);
            });
        });
    });

    describe("Double Ampersand (COND_CALL)", () => {

        it("should split '&&' (conditional call)", () => {

            const input  = "echo foo && echo bar",
                  output = ["echo foo", "echo bar"];

            assert.deepEqual(DOS.split_command(input), output);
        });
    });
});
