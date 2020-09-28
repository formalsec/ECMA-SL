const Expr = require("../Expr/Expr"); 

function MakeCondition(Stmt){
	class Condition extends Stmt {
		constructor(expr,then_block, else_block){
			super();
			this.expr = expr;
			this.then_block = then_block;
			this.else_block = else_block;
		}
		toString(){
			var else_str = this.else_block ? ("else {\n" + this.else_block.toString() + "\n}") : "";
			return "if(" + this.expr.toString() + ") {\n" +this.then_block.toString() + "\n}" + else_str;
		}

		interpret(config){
			var v = this.expr.interpret(config.store);
			var stmt_result = [];
			if(v){
				stmt_result = [this.then_block];
			} else{
				if(this.else_block){
					stmt_result = [this.else_block];
				}
			}
			config.cont = stmt_result.concat(config.cont.slice(1));	
			return config;
		}
	}

	Condition.fromJSON = function(obj){
		var expr = Expr.fromJSON(obj.expr);
		var then_block = Stmt.fromJSON(obj.then);
		if(obj.else){ 
			var else_block = Stmt.fromJSON(obj.else);
			return new Condition(expr,then_block,else_block);
		} else {
			return new Condition(expr,then_block,null);
		}
	}
	return Condition;
}

module.exports= MakeCondition;