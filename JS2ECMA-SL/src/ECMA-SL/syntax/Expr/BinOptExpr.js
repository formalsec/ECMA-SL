
const Oper = require("../Oper");

function MakeBinOptExpr(Expr){
  class BinOptExpr extends Expr {
    constructor(operator, expr_lhs, expr_rhs ) {
      super();
      this.operator = operator;
      this.expr_lhs = expr_lhs;
      this.expr_rhs = expr_rhs;

    }

    interpret(store){
      var v1 = this.expr_lhs.interpret(store);
      var v2 = this.expr_rhs.interpret(store);
      return this.operator.interpret(v1,v2); 
    }
  }

    BinOptExpr.fromJSON = function(obj){
    	expr_lhs =  Expr.fromJSON(obj.lhs);
      console.log(expr_lhs);
    	expr_rhs =  Expr.fromJSON(obj.rhs);
      console.log(expr_rhs);
    	oper = Oper.fromJSON(obj.op);
    	return new BinOptExpr(oper,expr_lhs, expr_rhs);
  }
  return BinOptExpr;
}

module.exports = MakeBinOptExpr;