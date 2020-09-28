const Expr = require("../Expr/Expr"); 


function MakeFieldAssign(Stmt){
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
  FieldAssign.fromJSON = function(obj) {
  	var expr_obj = Expr.fromJSON(obj.obj);
  	var expr_field = Expr.fromJSON(obj.field);
  	var expr_value = Expr.fromJSON(obj.value);
  	return new FieldAssign(expr_obj,expr_field,expr_value);

  }
  return FieldAssign;
}

module.exports = MakeFieldAssign;
