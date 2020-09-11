const Stmt = require("./Stmt")

class Return extends Stmt {
  constructor(expression) {
    super()
    this.expression = expression
  }

  toString() {
    return `return ${this.expression.toString()}`
  }
}

module.exports = Return
