const Expr = require("../Expr/Expr");

function MakePrint(Stmt){

	class Print extends Stmt {
		constructor(expression){
			super();
			this.expression = expression;
		}

		interpret(config){
			var v = this.expression.interpret(config.store);
			console.log("PRINT> "+ v +"/n");
			return config;
		}
	}

	Print.fromJSON = function(obj){
		var expr = Expr.fromJSON(obj.expr);
		return new Print(expr);
	}
	return Print;
}

module.exports = MakePrint;