const Expr = require("../Expr/Expr");

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
	  	config.cont=config.cont.slice(1);
	  	var frame = config.cs.pop();
	  	if (config.cs.lenghth > 1){
	  		config.store = frame.store;
	  		config.cont = frame.cont;
	  		config.store.sto[frame.var]=this.expression;
	  	}
	  	else{
	  		config.final_return = this.expression.interpret(config.store);
	  	}
	  	return config;
	  	
	  }
	}
	Return.fromJSON = function(obj) {
		var expr = Expr.fromJSON(obj.expr);
		return new Return(expr);
	}
	return Return;
}

module.exports = MakeReturn
