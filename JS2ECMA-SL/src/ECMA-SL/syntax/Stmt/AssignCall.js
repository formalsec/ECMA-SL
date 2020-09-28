const Expr = require("../Expr/Expr"); 
const Store = require("../../Store");
const CsFrame = require("../../CsFrame");

function MakeAssignCall(Stmt){
  class AssignCall extends Stmt {
    constructor(stringvar, func, args) {
      super();
      this.stringvar = stringvar;
      this.func = func;
      this.args = args;
      
    }
    toString(){
      var args_str = this.args.map(f => f.toString()); 
      return this.stringvar + " := " + this.func.toString() + "( " + args_str.join(", ") + " )";
    }

    interpret(config){
      var func_name = this.func.interpret(config.store);
      var vs = this.args.map(e => e.interpret(config.store));
      var f = config.prog.getFunc(func_name.value);
      console.log("1");
      if(f){

        var new_store = new Store(f.params, vs); 
        config.cs.push(new CsFrame(this.stringvar, config.stmtlist, config.store));
        config.store = new_store;
        config.cont = [f.body];
        return config
      }
      config.cont=config.cont.slice(1);
      return config;
    }

    
  }
  AssignCall.fromJSON = function(obj) {
  	var stringvar = obj.lhs;
  	var func = Expr.fromJSON(obj.func);
  	var args = obj.args.map(Expr.fromJSON);
  	return new AssignCall(stringvar,func,args);

  }
  return AssignCall;
}

module.exports = MakeAssignCall;
