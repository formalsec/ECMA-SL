const Expr = require("../Expr/Expr");

function MakeFieldDelete(Stmt){
	 
	class FieldDelete extends Stmt {
	  constructor(expressionObject, expressionField) {
	    super();
	    this.expressionObject = expressionObject;
	    this.expressionField = expressionField;
	    }

	}
	FieldDelete.fromJSON = function(obj) {
		var expr_obj = Expr.fromJSON(obj.obj);
		var expr_field = Expr.fromJSON(obj.field);
		return new FieldDelete(expr_obj,expr_field);

	}
	return FieldDelete;
}

module.exports = MakeFieldDelete;
