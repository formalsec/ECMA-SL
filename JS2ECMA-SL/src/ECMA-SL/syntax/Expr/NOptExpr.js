const Oper = require("../Oper");

function MakeNOptExpr(Expr){
  class NOptExpr extends Expr {
    constructor(n_aryOperator, expressionsList = []) {
      super();
      this.n_aryOperator = n_aryOperator;
      this.expressionsList = expressionsList;
    }

    toString() {
      return this.n_aryOperator.toString(
        this.expressionsList.map((expr) => expr.toString())
      );
    }

    interpret(store){
      var v_list = this.expressionsList.map(interpret(store));
      return this.n_aryOperator.interpret(v_list); 
    }
  }

  NOptExpr.fromJSON = function(obj){
    args = obj.args.map(Expr.fromJSON);
    oper = Oper.fromJSON(obj.op);
    return new NOptExpr(oper,args);
  }
  return NOptExpr;
}

module.exports = MakeNOptExpr;
