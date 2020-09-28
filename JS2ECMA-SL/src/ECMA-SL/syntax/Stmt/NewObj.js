
class NewObj extends Expr {
  constructor(fieldValueList = []) {
    super();
    this.fieldValueList = fieldValueList;
  }

  toString() {
    return `{ ${this.fieldValueList
      .map((fieldValue) => fieldValue.toString())
      .join(", ")} }`;
  }
}

NewObj.FieldValue = class {
  constructor(fieldExpression, valueExpression) {
    this.fieldExpression = fieldExpression;
    this.valueExpression = valueExpression;
  }

  toString() {
    return `${this.fieldExpression.toString()}: ${this.valueExpression.toString()}`;
  }
};

module.exports = NewObj;
