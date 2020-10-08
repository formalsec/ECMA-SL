function MakeTupleVal(Val){
	class TupleVal extends Val{
	  constructor(value) {
	  	super();
	    this.value = value;
	  }
	}

	 TupleVal.fromJSON = function(value) {
	 	return new TupleVal(value);
	}
	return TupleVal;
}

module.exports = MakeTupleVal;