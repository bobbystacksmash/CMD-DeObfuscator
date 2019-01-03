const assert = require("chai").assert,
      CMD    = require("../index");

describe("Command identification tests", () => {

    it("should correctly identify a command with a leading pathspec", () => {

        const commands = [
            { input: "wscript",    output: "wscript" },
            { input: "   cmd.exe ", output: "cmd.exe" },
            { input: "..\\..\\..\\cmd.exe", output: "cmd.exe" },
            { input: "C:\\Windows\\System32\\cmd.exe", output: "cmd.exe" },
            { input: `"C:\\Windows\\System32\\cmd.exe"`, output: "cmd.exe" },
            { input: "C:evil.exe", output: "evil.exe" }
        ];

        commands.forEach(
            cmd => assert.equal(CMD.try_identify_command(CMD.tokenise(cmd.input)), cmd.output)
        );
    });
});
