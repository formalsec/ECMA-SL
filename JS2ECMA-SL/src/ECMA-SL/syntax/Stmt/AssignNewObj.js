function MakeAssignNewObj(Stmt){
	class AssignNewObj extends Stmt {
	  constructor(stringvar) {
	    super();
	    this.stringvar = stringvar;
	  }
	}

	AssignNewObj.fromJSON = function(obj) {
		stringvar = obj.lhs;
		
		return new AssignNewObj(stringvar);

	}
	return AssignNewObj;
}

module.exports = MakeAssignNewObj;
