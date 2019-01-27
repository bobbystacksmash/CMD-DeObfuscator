const assert           = require("chai").assert,
      tokenise         = require("../../lib/tokeniser"),
      interpret        = require("../../lib/interpreter").interpreter,
      identify_command = require("../../lib/interpreter").try_identify_command,
      getcmd           = require("../../lib/commands");

const util = {
    filterEOF: (tokens) => tokens.filter(t => t.name !== "EOF"),
    lexemes:   (tokens) => util.filterEOF(tokens).map(t => t.text),
    names:     (tokens) => util.filterEOF(tokens).map(t => t.name)
};

describe("Commands", () => {

    describe("Nested CMD instances", () => {

        it("should identify new cmd contexts", () => {

            const input  = `cmd cmd cmd`,
                  context = [],
                  output = [];

            let ident   = identify_command(tokenise(input)),
                handler = getcmd(ident);


        });
    });

    describe("SET", () => {

        it("should SET new environment variables", () => {

            const input   = `SET foo=bar`,
                  context = { vars: {} },
                  output  = { vars: { foo: "bar" } };

            let ident = identify_command(tokenise(input));

        });
    });
});
