function MakePrimitiveVal(Val){
  class PrimitiveVal extends Val {
    constructor(value) {
    	super();
      this.value = value;
    }

    toString() {
      if ((typeof this.value) === "string") {
        return `"${this.value.split('"').join('\\"')}"`;
      }
      
      return ("" + this.value);
    }
  }

  PrimitiveVal.fromJSON = function(value) {
   	return new PrimitiveVal(value);
  }
  return PrimitiveVal;
}

module.exports = MakePrimitiveVal;