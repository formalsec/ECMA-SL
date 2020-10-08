function MakeTypeVal(Val){
	class TypeVal extends Val{
  		constructor(value) {
  			super();
    		this.value = value;
	  }
	}

	 TypeVal.fromJSON = function(value) {
	 	return new TypeVal(value);
	 	
	}
	return TypeVal;
}
module.exports = MakeTypeVal;