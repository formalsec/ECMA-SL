const Expr = require("../Expr/Expr");
const EmptyLab = require("../Labels/EmptyLab");

function MakeReturn(Stmt){
	
	class Return extends Stmt {
	  constructor(expression) {
	    super();
	    this.expression = expression
	  }

	  toString() {
	    return `return ${this.expression.toString()}`
	  }

	  interpret(config){
	  	config.cont=[];
	  	var frame = config.cs.pop();
	  	if (config.cs.lenghth > 1){
	  		config.store = frame.store;
	  		config.cont = frame.cont;
	  		config.store.sto[frame.var]=this.expression;
	  	}
	  	else{
	  		config.final_return = this.expression.interpret(config.store);
	  	}
	  	return {config : config, seclabel: new EmptyLab()};
	  	
	  }
	}
	Return.fromJSON = function(obj) {
		var expr = Expr.fromJSON(obj.expr);
		return new Return(expr);
	}
	return Return;
}

module.exports = MakeReturn
