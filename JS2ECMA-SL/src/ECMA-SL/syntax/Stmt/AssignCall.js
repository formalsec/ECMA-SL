const Expr = require("../Expr/Expr"); 
const Store = require("../../Store");
const CsFrame = require("../../CsFrame");
const AssignCallLab = require("../Labels/AssignCallLab");
const Interceptor = require("../Interceptor");

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

    toJS(){
      console.log("AssignCall " + this.func + " toJS");
      var args_js = this.args.map((arg) => arg.toJS());
      return {
        "type": "ExpressionStatement",
        "expression": {
          "type": "AssignmentExpression",
          "operator": "=",
          "left": {
            "type": "Identifier",
            "name": this.stringvar
          },
          "right": {
            "type": "CallExpression",
            "callee": {
              "type": "Identifier",
              "name": this.func 
            },
            "arguments": args_js
          }
        }
      }
    }

    interpret(config){
      console.log(">ASSIGN CALL");
      config.cont=config.cont.slice(1);
      var func_name = this.func.interpret(config.store);
      var f = config.prog.getFunc(func_name.value);
      var vs = this.args.map(e => e.interpret(config.store));
      
      if(f){
        var new_store = new Store(f.params, vs);
        config.cs.push(new CsFrame(this.stringvar, config.cont, config.store));
        config.store = new_store;
        config.cont = [f.body];
        
      }else{
        //interceptor
        var label = Interceptor.search(func_name.value, vs.map(v => v.value), this.args);
        if(label != undefined)
          return {config : config, seclabel: label};
          


      }
      
      return {config : config, seclabel: new AssignCallLab(this.stringvar, f, this.args)};
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
