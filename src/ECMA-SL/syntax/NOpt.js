const Expr = require("./Expr");

class NOpt extends Expr {
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
}

NOpt.ListExpr = class {
  toString(elements = []) {
    // Array.prototype.join concatenates the empty string when null or undefined appear.
    return `[ ${elements
      .map((elem) => {
        if (elem === null || elem === undefined) {
          return String(elem);
        }
        return elem;
      })
      .join(", ")} ]`;
  }
};

module.exports = NOpt;
