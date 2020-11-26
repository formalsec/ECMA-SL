const Expr = require("../Expr/Expr"); 
const FieldAssignLab = require("../Labels/FieldAssignLab");


function MakeFieldAssign(Stmt){
  class FieldAssign extends Stmt {
    constructor(expressionObject, expressionField, expressionValue) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.expressionValue = expressionValue;
    }

    toString() {
      return `${this.expressionObject.toString()}[${this.expressionField.toString()}] := ${this.expressionValue.toString()}`;
    }

    interpret(config) {
      console.log('>FIELD ASSIGN');
      config.cont = config.cont.slice(1) ;
      var object = this.expressionObject.interpret(config.store).value;
      var field = this.expressionField.interpret(config.store).value;
      config.heap.setFieldValue(object, field, this.expressionValue.interpret(config.store));
      return {config : config, seclabel: new FieldAssignLab(object, field, this.expressionObject, this.expressionField, this.expressionValue)};
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
