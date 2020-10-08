const UpgVarLab = require("./Labels/UpgVarLab");

class Interceptor{

}

Interceptor.search = function(func_name, vs){
	//console.log(">INTERCEPTOR");
	//console.log(vs);
		switch(func_name){
		case "upgVar": return new UpgVarLab(vs[0], vs[1]);
		case "upgStructVal": return new UpgStructValLab(vs);
		default: throw new Error("Unkown function");
	}
}

module.exports = Interceptor; 