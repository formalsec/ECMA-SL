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
  .option("name", {
    alias: "n",
    description: "Name of the function that creates the AST object",
    type: "string"
  })
  .demandOption("input")
  .usage("Usage: $0 -i [filepath]")
  .help()
  .alias("help", "h").argv;

fs.readFile(argv.input, "utf-8", (err, data) => {
  if (err) throw err;
  const FUNC_NAME = argv.name ? argv.name : "buildAST";

  let prog;
  try {
    prog = esprima.parseScript(data);
  } catch(ex) {
    prog = newEarlySyntaxError(ex.description)
  }

  const statements = translator.fromJSObjectToESLStatements(prog);
  const func = translator.fromESLStatementsToESLFunction(
    FUNC_NAME,
    ["___internal_esl_global"],
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

function newEarlySyntaxError(message) {
  return {
    sourceType: "script",
    type: "EarlySyntaxError",
    message: message || ""
  };
}
