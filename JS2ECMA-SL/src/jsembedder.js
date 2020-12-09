
// node parse_esl <file_to_parse>
const escodegen = require("escodegen");
const { PerformanceObserver, performance } = require('perf_hooks');
const Prog = require("./ECMA-SL/syntax/Prog");
const _EXCEPTION_INTERRUPTION_ = "__exception_interruption__";
const option = {
    format : {
      quotes : 'single',
      indent : {
        style : '\t'
      }
    }
}

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
    var js_prog = prog.toJS();
    var js_prog_emb = escodegen.generate(js_prog, option);
    console.log(js_prog_emb);
    console.log("===============================\n");
    var t_execution0 = performance.now()
    var result = eval(js_prog_emb);
    var t_execution1 = performance.now()
    if(result != _EXCEPTION_INTERRUPTION_){
      if(Array.isArray(result)){
        console.log("MAIN return -> "+ result[0]);
      }
      else{
        console.log("MAIN return -> "+ result);
      }
    }
    console.log("\n===============================\nInterpretation Time : " + (t_execution1 - t_execution0));
    

   
}


