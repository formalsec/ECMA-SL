const esprima = require("esprima-next");
const yargs = require("yargs");
const fs = require("fs");
const translator = require("./ECMA-SL/translator");
const ParseRegExps = require("./ECMA-SL/utils/parse_regexp");

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
    type: "string",
  })
  .option("compile-to-core", {
    alias: "c",
    description: "Compiles the JS input file directly to Core",
    type: "boolean",
    default: false,
  })
  .option("optimised", {
    description: 'Adds the "optimised" property to the generated AST object',
    type: "boolean",
    default: false,
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
    progObj = esprima.parseScript(data);
    prog = ParseRegExps(progObj);
  } catch (ex) {
    prog = newEarlySyntaxError(ex.description);
  }

  if (argv.optimised) {
    prog.optimised = true;
  }

  const statements = translator.fromJSObjectToESLStatements(
    prog,
    argv.compileToCore
  );
  const func = translator.fromESLStatementsToESLFunction(
    FUNC_NAME,
    argv.compileToCore ? ["___internal_esl_global"] : [],
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
