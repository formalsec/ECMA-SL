const Stmt = require("./Stmt");

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

module.exports = Assign;
