const Stmt = require("./Stmt");
const Expr = require("./Expr"); 

class Assign extends Stmt {
  constructor(variable, expression) {
    super();
    this.variable = variable;
    this.expression = expression;
  }

  toString() {
    return `${this.variable.toString()} := ${this.expression.toString()}`;
  }
}

Assign.fromJSON = function(obj) {
  var var_name = obj.var_name; 
  var expr = Expr.fromJSON(obj.expr); 
  return new Assign(var_name, expr);
}

module.exports = Assign;
