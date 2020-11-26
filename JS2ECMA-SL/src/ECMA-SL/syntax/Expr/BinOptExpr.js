
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
      console.log("++BINOPT");

      var v1 = this.expr_lhs.interpret(store);
      var v2 = this.expr_rhs.interpret(store);
      console.log("*********** DEBUG BINOPT *********");
      console.log(this.operator);
      console.log(v1);
      console.log(v2);
      console.log("***************************");
      return this.operator.interpret(v1,v2); 
    }
    getVars(){
      var vars = this.expr_lhs.getVars().concat(this.expr_rhs.getVars());
      return vars; 
    }
  }

    BinOptExpr.fromJSON = function(obj){
    	expr_lhs =  Expr.fromJSON(obj.lhs);
    	expr_rhs =  Expr.fromJSON(obj.rhs);
    	oper = Oper.fromJSON(obj.op);
    	return new BinOptExpr(oper,expr_lhs, expr_rhs);
  }
  return BinOptExpr;
}

module.exports = MakeBinOptExpr;