const Expr = require("../Expr/Expr");

function MakeException(Stmt){

	class Exception extends Stmt {
		constructor(string){
			super();
			this.string = string;
		}
	toString() {
		return `throw ${this.string}`
	}

	toJS(){
		return {
      "type": "ExpressionStatement",
      "expression": {
        "type": "CallExpression",
        "callee": {
          "type": "MemberExpression",
          "computed": false,
          "object": {
            "type": "Identifier",
            "name": "console"
          },
          "property": {
            "type": "Identifier",
            "name": "log"
          }
        },
        "arguments": [
          {
            "type": "Literal",
            "value": this.string,
            "raw": "\""+this.string+"\""
          }
        ]
      }
    },
    {
      "type": "ExpressionStatement",
      "expression": {
        "type": "CallExpression",
        "callee": {
          "type": "MemberExpression",
          "computed": false,
          "object": {
            "type": "Identifier",
            "name": "process"
          },
          "property": {
            "type": "Identifier",
            "name": "exit"
          }
        },
        "arguments": [
          {
            "type": "Literal",
            "value": 1,
            "raw": "1"
          }
        ]
      }
    }
	}

	interpret(config){
		console.log(">EXCEPTION");
		console.log(this.string);
		process.exit(1)
	}
}

	Exception.fromJSON = function(obj){
		var string = obj.value;
		return new Exception(string);
	}
	return Exception;
}

module.exports = MakeException;