const Expr = require("../Expr/Expr");
const FieldDeleteLab = require("../Labels/FieldDeleteLab");

function MakeFieldDelete(Stmt){
	 
	class FieldDelete extends Stmt {
	  	constructor(expressionObject, expressionField) {
		    super();
		    this.expressionObject = expressionObject;
		    this.expressionField = expressionField;
		    }

    	interpret(config){
    		config.cont=config.cont.slice(1);
    		var object = this.expressionObject.interpret(config.store).value;
      		var field = this.expressionField.interpret(config.store).value;
    		config.heap.deleteField(object, field);
    	return {config : config, seclabel: new FieldDeleteLab(object, field, this.expressionObject, this.expressionField)};

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
