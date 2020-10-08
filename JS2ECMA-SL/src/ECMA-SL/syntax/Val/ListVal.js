const Val= require("./Val");

function MakeListVal(Val){
	class ListVal extends Val{
	  constructor(list) {
	  	super();
	    this.list = list;
	  }
	  
	  getMember(index){
	  	return this.list[index];
	  }

	  getTail(){
	  	return new ListVal(this.list.slice(1));
	  }
	}

	ListVal.fromJSON = function(list) {
		return new ListVal(list);
	}
	return ListVal;
}
module.exports = MakeListVal;