const Store = require('./ECMA-SL/Store');
const CsFrame = require("./ECMA-SL/CsFrame");
const Heap = require("./ECMA-SL/syntax/Heap");

class Interpreter {

}

Interpreter.iterate = function(config, sec_conf){
	if(config.cont.length > 0){
		//console.log("\ninterpret -> " + config.cont);
		var stmt = config.cont[0];
		result = stmt.interpret(config);
		mon_result= result.seclabel.interpret(sec_conf);
		if(mon_result.error){
			console.log(mon_result.error);
			exit();
		}
		this.iterate(result.config, mon_result); 
	}
	return config.final_return;
}

Interpreter.interpretProg = function(_prog){
	//Creating initial conditions
	console.log("=========== Running ===========\n" + _prog + "\n===============================\n")
	var main_func= _prog.getFunc('main');
  	var final_value = this.iterate({prog:_prog, cs:[new CsFrame()], store: new Store([],[]), cont : [main_func.body], heap: new Heap()}, {ssto:new Store ([],[]), sheap: new Heap(), scs:[new CsFrame()], pc : [false] }); //fazer funcao 
  	console.log("MAIN return -> "+ final_value);
}

module.exports = Interpreter;