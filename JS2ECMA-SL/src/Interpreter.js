const Store = require('./ECMA-SL/Store');
const SecStore = require('./ECMA-SL/SecStore');
const CsFrame = require("./ECMA-SL/CsFrame");
const Heap = require("./ECMA-SL/syntax/Heap");
const SecHeap = require("./ECMA-SL/syntax/SecHeap");
const Lattice = require("./ECMA-SL/syntax/Lattice");
const ValModule = require("./ECMA-SL/syntax/Val/Val");
const TupleVal = ValModule.TupleVal;
const _EXCEPTION_INTERRUPTION_ = "__exception_interruption__";
const { PerformanceObserver, performance } = require('perf_hooks');

class Interpreter {

}

Interpreter.iterate = function(config, sec_conf, mon){
	if(config.cont.length > 0){
		if(config.exit){
			config.final_return = _EXCEPTION_INTERRUPTION_;
			return _EXCEPTION_INTERRUPTION_;
		}
		var stmt = config.cont[0];
		result = stmt.interpret(config);

		if(mon != undefined){
			mon_result= result.seclabel.interpret(sec_conf);
			if(mon_result.error != undefined){
				console.log("MONITOR EXCEPTION -> " + mon_result.error+"\n\n===============================");
				config.final_return = _EXCEPTION_INTERRUPTION_;
				return _EXCEPTION_INTERRUPTION_;
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
	var t_execution0 = performance.now()
  	var final_value = this.iterate({exit: false, prog:_prog, cs:[new CsFrame()], store: new Store([],[]), cont : [main_func.body], heap: new Heap()}, {ssto:new SecStore([],[]), sheap: new SecHeap(), scs:[new CsFrame()], pc : [Lattice.bottom()] }, mon); //fazer funcao 
  	var t_execution1 = performance.now()
  	if (final_value != _EXCEPTION_INTERRUPTION_){
	  	if (final_value instanceof TupleVal) {
	  		console.log("MAIN return -> "+ final_value.value[0] +"\n\n===============================");
	  	} else {
	  		console.log("MAIN return -> "+ final_value +"\n\n===============================");
	  	}
	  }
  	console.log("Interpretation Time : " + (t_execution1 - t_execution0));
}

module.exports = Interpreter;