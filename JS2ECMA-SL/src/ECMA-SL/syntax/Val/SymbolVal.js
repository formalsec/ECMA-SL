function MakeSymbolVal(Val){
	class SymbolVal extends Val{
	  constructor(value) {
	  	super();
	    this.value = value;
	  }

	  toJS(){
	  	console.log("HELLLOI");
	  	console.log(this.value);
      return {
        "type": "Identifier",
        "name": "undefined"
      }
    }
	}

	 SymbolVal.fromJSON = function(value) {
	 	return new SymbolVal(value);
	}
	return SymbolVal;
}
module.exports = MakeSymbolVal;