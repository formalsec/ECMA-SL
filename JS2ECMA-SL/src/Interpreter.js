const Store = require('./ECMA-SL/Store');
const SecStore = require('./ECMA-SL/SecStore');
const CsFrame = require("./ECMA-SL/CsFrame");
const Heap = require("./ECMA-SL/syntax/Heap");
const SecHeap = require("./ECMA-SL/syntax/SecHeap");
const Lattice = require("./ECMA-SL/syntax/Lattice");
class Interpreter {

}

Interpreter.iterate = function(config, sec_conf, mon){
	if(config.cont.length > 0){
		var stmt = config.cont[0];
		result = stmt.interpret(config);
		if(mon != undefined){
			mon_result= result.seclabel.interpret(sec_conf);
			if(mon_result.error != undefined){
				console.log("MONITOR EXCEPTION -> " + mon_result.error);
				process.exit(1); 
			}
			result.sec_conf = mon_result;
		}
		this.iterate(result.config, result.sec_conf, mon); 
		
	}
	return config.final_return;
}

Interpreter.interpretProg = function(_prog, mon){
	//Creating initial conditions
	console.log("=========== Running ===========\n" + _prog + "\n===============================\n")
	var main_func= _prog.getFunc('main');
  	var final_value = this.iterate({prog:_prog, cs:[new CsFrame()], store: new Store([],[]), cont : [main_func.body], heap: new Heap()}, {ssto:new SecStore([],[]), sheap: new SecHeap(), scs:[new CsFrame()], pc : [Lattice.bottom()] }, mon); //fazer funcao 
  	console.log("MAIN return -> "+ final_value);
}

module.exports = Interpreter;