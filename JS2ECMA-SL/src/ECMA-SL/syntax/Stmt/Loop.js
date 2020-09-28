
const Expr = require("../Expr/Expr");

function MakeLoop(Stmt){
	
	class Loop extends Stmt {
		  constructor(expr, block) {
		    super();
		    this.expr = expr;
		    this.block = block;
		}

		interpret(config){
			var v = this.expr.interpret(config.store);
			if(v){
				config = this.block.interpret(config);
				interpret(config);
			}
			return config;
		}

	}

	Loop.fromJSON = function(obj){
		var expr = Expr.fromJSON(obj.expr);
		var block = Stmt.fromJSON(obj.do);
		return new Loop(expr,block) 
	}
	return Loop;
}
module.exports = MakeLoop;
