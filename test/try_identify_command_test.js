const assert = require("chai").assert,
      CMD    = require("../index");

describe("Command identification tests", () => {

    it("should identify the command and the offset", () => {

        let cmd    = `cmd "set foo=bar"`,
            tokens = CMD.tokenise(cmd);

        assert.deepEqual(
            CMD.try_identify_command(tokens),
            {
                command: "cmd",
                offset:  3
            }
        );
    });

    it("should correctly identify the command", () => {

        const commands = [
            { input: "wscript",    output: "wscript" },
            { input: "   cmd.exe ", output: "cmd" },
            { input: "..\\..\\..\\cmd.exe", output: "cmd" },
            { input: "C:\\Windows\\System32\\cmd.exe", output: "cmd" },
            { input: `"C:\\Windows\\System32\\cmd.exe"`, output: "cmd" },
            { input: "C:evil.exe", output: "evil" },
            { input: "SET foo=bar", output: "set" },
        ];

        commands.forEach(
            cmd => assert.equal(CMD.try_identify_command(CMD.tokenise(cmd.input)).command, cmd.output)
        );
    });

    it("should correctly identify the index after the command", () => {

        const commands = [
            { input: `wscript`,     offset: 7 },
            { input: `"cmd"`,       offset: 5 },
            { input: `"C:cmd.exe"`, offset: 11 },
            { input: `C:\\foo.exe`, offset: 11 },
            { input: `cmd "set foo=bar"`, offset: 3 }
        ];

        commands.forEach(cmd => {
            let tokens = CMD.tokenise(cmd.input);
            assert.equal(CMD.try_identify_command(tokens).offset, cmd.offset);
        });
    });
});
