const Expr = require("../Expr/Expr"); 

function MakeAssignInObjCheck(Stmt){
  class AssignInObjCheck extends Stmt {
    constructor(stringvar, expressionObject, expressionField) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.stringvar = stringvar;
    }

   
  }
  AssignInObjCheck.fromJSON = function(obj) {
  	var expr_obj = Expr.fromJSON(obj.obj);
  	var expr_field = Expr.fromJSON(obj.field);
  	var stringvar = obj.lhs;
  	return new AssignInObjCheck(stringvar,expr_obj,expr_field);

  }
  return AssignInObjCheck;
}
module.exports = MakeAssignInObjCheck;
