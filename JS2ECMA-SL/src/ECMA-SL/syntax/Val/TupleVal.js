function MakeTupleVal(Val){
	class TupleVal extends Val{
	  constructor(value) {
	  	super();
	    this.value = value;
	  }

	  toJS(){
	  	var list_js = this.value.map((element) => element.toJS());
	  	return list_js;
	  }
	}

	 TupleVal.fromJSON = function(value) {
	 	return new TupleVal(value);
	}
	 

	return TupleVal;
}

module.exports = MakeTupleVal;