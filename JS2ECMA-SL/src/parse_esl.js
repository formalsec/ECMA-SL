
// node parse_esl <file_to_parse>
const Prog = require("./ECMA-SL/syntax/Prog");
const Interpreter = require("./Interpreter");
const { PerformanceObserver, performance } = require('perf_hooks');

fs = require('fs');


var file_to_parse  = process.argv[2];
var mon = process.argv[3];
console.log(file_to_parse);
fs.readFile(file_to_parse, 'utf8', parseFile);

function parseFile(err,obj) {
	console.log(mon);
	console.log("Parsing File...");
	var t_parse0 = performance.now()
	let jsonprog = JSON.parse(obj);
	var t_parse1 = performance.now()
	console.log("Parsing... [1/2] ");
	var t_parse2 = performance.now()
    var prog = Prog.fromJSON(jsonprog);
    var t_parse3 = performance.now()
    console.log("Parsing... [2/2] ");
    console.log("Parsing complete.");
    //console.log(prog.toString());
    // ... chamar o interpretador 
	Interpreter.interpretProg(prog, mon);
	console.log("Parsing Time : " + ((t_parse1 - t_parse0) + (t_parse3 - t_parse2)));
}	


