const Expr = require("./Expr");

class Var extends Expr {
  constructor(varStr) {
    super();
    this.variable = varStr;
  }

  toString() {
    return this.variable;
  }
}

module.exports = Var;
