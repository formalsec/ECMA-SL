const Val = require("../Val/Val");

function MakeValExpr(Expr){
	class ValExpr extends Expr {
	  constructor(value) {
	    super();
	    this.value = value;

	  }

	  toString(){
	  	return this.value.toString();
	  }

	  interpret(store){
	  	return this.value;
	  }

	  getVars(){
	  	return [];
	  }
	}

	ValExpr.fromJSON = function(obj) {
		var v = Val.fromJSON(obj.value);
		return new ValExpr(v);
	}
	return ValExpr;
}

module.exports = MakeValExpr;