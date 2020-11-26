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