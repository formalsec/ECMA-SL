const Store = require("../../Store");
const Heap = require("../Heap");
const Val =require("../Val/Val");
const LocationVal = require("../Val/LocationVal")(Val);
const EmptyLab = require("../Labels/EmptyLab");

function MakeAssignNewObj(Stmt){
	class AssignNewObj extends Stmt {
	  constructor(stringvar) {
	    super();
	    this.stringvar = stringvar;
	  }

	  interpret(config){
	  	var obj_name = config.heap.createObject();
	  	config.store.sto[this.stringvar] = new LocationVal(obj_name);
	  	config.cont=config.cont.slice(1);
	  	return {config : config, seclabel: new EmptyLab()};
	  }
	}

	AssignNewObj.fromJSON = function(obj) {
		stringvar = obj.lhs;
		
		return new AssignNewObj(stringvar);

	}

	return AssignNewObj;
}

module.exports = MakeAssignNewObj;
