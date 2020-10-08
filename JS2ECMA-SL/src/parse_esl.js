
// node parse_esl <file_to_parse>
const Prog = require("./ECMA-SL/syntax/Prog");
const Interpreter = require("./Interpreter");
fs = require('fs');


var file_to_parse  = process.argv[2];
console.log(file_to_parse);
fs.readFile(file_to_parse, 'utf8', parseFile);

function parseFile(err,obj) {
	console.log("Parsing File...");
	let jsonprog = JSON.parse(obj);
	console.log("Parsing... [1/2] ");
    var prog = Prog.fromJSON(jsonprog);
    console.log("Parsing... [2/2] ");
    console.log("Parsing complete.");
    //console.log(prog.toString());
    // ... chamar o interpretador 
	Interpreter.interpretProg(prog);
}


