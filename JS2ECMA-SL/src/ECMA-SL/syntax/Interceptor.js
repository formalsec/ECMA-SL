const UpgVarLab = require("./Labels/UpgVarLab");
const UpgPropExistsLab = require("./Labels/UpgPropExistsLab");
const UpgStructValLab = require("./Labels/UpgStructValLab");
class Interceptor{

}

Interceptor.search = function(func_name, vs, exprs){
	//console.log(">INTERCEPTOR");
	//console.log(vs);
	//Arg number check for each
		switch(func_name){
		case "upgVar": return new UpgVarLab(vs[0], vs[1]);
		case "upgStructVal": return new UpgStructValLab(vs[0], exprs[0], vs[1]);
		case "upgStructExists": return new UpgStructValLab(vs[0], exprs[0], vs[1]);
		case "upgPropExists": return new upgPropExistsLab(vs[0], vs[1], exprs[0], exprs[1], vs[2]);
		case "upgPropVal": return new upgPropExistsLab(vs[0], vs[1], exprs[0], exprs[1], vs[2]);
		default: throw new Error("Unkown function");
	}
}

module.exports = Interceptor; 