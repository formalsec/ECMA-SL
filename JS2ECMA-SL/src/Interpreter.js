const Store = require('./ECMA-SL/Store');
const CsFrame = require("./ECMA-SL/CsFrame");
const Heap = require("./ECMA-SL/syntax/Heap");

class Interpreter {

}

Interpreter.iterate = function(config){
	if(config.cont.length > 0){
		var stmt = config.cont[0];
		config = stmt.interpret(config);
		this.iterate(config);
	}
	return config.final_return;
}

Interpreter.interpretProg = function(_prog){
	//Creating initial conditions
	console.log("=========== Running ===========\n" + _prog + "\n===============================\n")
	var main_func= _prog.getFunc('main');
  	var final_value = this.iterate({prog:_prog, cs:[new CsFrame()], store: new Store([],[]), cont : [main_func.body], heap: new Heap()}); 
  	console.log("MAIN return -> "+ final_value);
}

module.exports = Interpreter;