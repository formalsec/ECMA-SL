function MakeVoidVal(Val){

	class VoidVal extends Val{
	  constructor() {
	  	super();
	  }
	}

	 VoidVal.fromJSON = function() {
	 	return new VoidVal();
	}

	return VoidVal;
}
module.exports = MakeVoidVal;