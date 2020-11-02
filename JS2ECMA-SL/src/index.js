const esprima = require("esprima");
const yargs = require("yargs");
const fs = require("fs");
const translator = require("./ECMA-SL/translator");

const argv = yargs
  .option("input", { alias: "i", description: "JS input file", type: "string" })
  .option("output", {
    alias: "o",
    description: "ECMA-SL output file",
    type: "string",
  })
  .demandOption("input")
  .usage("Usage: $0 -i [filepath]")
  .help()
  .alias("help", "h").argv;

fs.readFile(argv.input, "utf-8", (err, data) => {
  if (err) throw err;

  const prog = esprima.parseScript(data);

  const statements = translator.fromJSObjectToESLStatements(prog);
  const func = translator.fromESLStatementsToESLFunction(
    "buildAST",
    [],
    statements
  );

  if (argv.output) {
    fs.writeFile(argv.output, func.toString(), "utf8", (err) => {
      if (err) throw err;
      console.log("The file has been saved!");
    });
  } else {
    console.log(func.toString());
  }
});
