const Oper = require("../Oper");

function MakeUnOptExpr(Expr){

	class UnOptExpr extends Expr {
	  constructor(operator, expr_rhs ) {
	    super();
	    this.operator = operator;
	    this.expr_rhs = expr_rhs;

	  }
	  toString(){
	  	return ("(" + this.operator.toString() + " " + this.expr_rhs.toString() + ")");
	  }

	  interpret(store){
	  	var v = this.expr_rhs.interpret(store);
	  	return this.operator.interpret(v); 
	  }
	  getVars(){
	  	return this.expr_rhs.getVars();
	  }
	}

	  UnOptExpr.fromJSON = function(obj){
	  	expr_rhs =  Expr.fromJSON(obj.rhs);
	  	oper = Oper.fromJSON(obj.op);
	  	return new UnOptExpr(oper, expr_rhs);
	}
	return UnOptExpr;
}

module.exports = MakeUnOptExpr;