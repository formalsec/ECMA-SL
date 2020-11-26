const Expr = require("../Expr/Expr");
const FieldLookupLab = require("../Labels/FieldLookupLab");

function MakeFieldLookup(Stmt){
  
  class FieldLookup extends Stmt {
    constructor(stringvar, expressionObject, expressionField) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.stringvar = stringvar;
    }

    interpret(config){
      console.log(">FIELD LOOKUP");
      console.log("*********** DEBUG *********");
      console.log(this.stringvar);
      console.log(this.expressionObject);
      console.log(this.expressionField);
      
      config.cont=config.cont.slice(1);
      var object = this.expressionObject.interpret(config.store).value;
      var field = this.expressionField.interpret(config.store).value;
      console.log(object);
      console.log(field);
      console.log(config.heap.getField(object, field));
      console.log("***************************");
      config.store.sto[this.stringvar] = config.heap.getField(object, field);
      
     return {config : config, seclabel: new FieldLookupLab(this.stringvar, object, field, this.expressionObject, this.expressionField)};
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
