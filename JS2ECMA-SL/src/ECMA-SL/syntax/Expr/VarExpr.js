function MakeVarExpr(Expr){

	class VarExpr extends Expr {
	  constructor(varStr) {
	    super();
	    this.variable = varStr;
	  }

	  toString() {
	    return this.variable;
	  }

	  interpret(store){
	  	return store.sto[this.variable];
	  }
	  getVars(){
	  	return [this.variable];
	  }
	}

	VarExpr.fromJSON = function(obj) {
		return new VarExpr(obj.name);
	}
	return VarExpr;
}
module.exports = MakeVarExpr;
