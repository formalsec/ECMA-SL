const Expr = require("../Expr/Expr"); 
const AssignLab = require("../Labels/AssignLab");

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
      var v = this.expression.interpret(config.store);
      config.store.sto[this.variable]=v;
      config.cont=config.cont.slice(1);
      return {config : config, seclabel: new AssignLab(this.variable, this.expression)};
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
