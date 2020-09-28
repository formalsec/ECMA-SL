const Expr = require("../Expr/Expr");

function MakeFieldLookup(Stmt){
  
  class FieldLookup extends Stmt {
    constructor(stringvar, expressionObject, expressionField) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.stringvar = stringvar;
    }

   
  }
  FieldLookup.fromJSON = function(obj) {
  	var expr_obj = Expr.fromJSON(obj.obj);
  	var expr_field = Expr.fromJSON(obj.field);
  	var stringvar = obj.lhs;
  	return new FieldLookup(stringvar,expr_obj,expr_field);

  }
  return FieldLookup;
}

module.exports = MakeFieldLookup;
