const EmptyLab = require("../Labels/EmptyLab");
function MakeBlock(Stmt){

	class Block extends Stmt {
	  constructor(stmtsList = []) {
	    super();
	    this.statements = stmtsList;
	  }

	  toString() {
	    return `${this.statements.map((stmt) => stmt.toString()).join(";\n")}`;
	  }
	  interpret(config){
	  	config.cont = this.statements.concat(config.cont.slice(1)) ;
      	return {config : config, seclabel: new EmptyLab()};
	  }
	}

	Block.fromJSON = function(obj){
		var stmts = obj.value.map(Stmt.fromJSON);
		return new Block(stmts) 
	}
	return Block;
}

module.exports = MakeBlock;
