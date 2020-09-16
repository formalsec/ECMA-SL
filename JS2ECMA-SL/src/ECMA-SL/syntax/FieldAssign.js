const Stmt = require("./Stmt");

class FieldAssign extends Stmt {
  constructor(expressionObject, expressionField, expressionValue) {
    super();
    this.expressionObject = expressionObject;
    this.expressionField = expressionField;
    this.expressionValue = expressionValue;
  }

  toString() {
    return `${this.expressionObject.toString()}["${this.expressionField.toString()}"] := ${this.expressionValue.toString()}`;
  }
}

module.exports = FieldAssign;
