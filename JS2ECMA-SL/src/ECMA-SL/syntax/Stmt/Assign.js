const Expr = require("../Expr/Expr"); 
function MakeAssign(Stmt){

  class Assign extends Stmt {
    constructor(variable, expression) {
      super();
      this.variable = variable;
      this.expression = expression;
    }

    toString() {
      return `${this.variable.toString()} := ${this.expression.toString()}`;
    }

    interpret(config){
      console.log("1");
      var v = this.expression.interpret(config.store);
      config.store.sto[this.variable]=v;
      config.cont=config.cont.slice(1);
      return config;
    }
  }

  Assign.fromJSON = function(obj) {
    var var_name = obj.lhs;
    var expr = Expr.fromJSON(obj.rhs); 
    return new Assign(var_name, expr);
  }
  return Assign;
}

module.exports = MakeAssign;
