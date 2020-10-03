const Lattice = require("../Lattice");

class AssingLab{
	constructor(stringvar, expr){
		this.stringvar = stringvar;
		this.expr = expr;
	}
	interpret(sec_conf){
		var vars = [];
		vars = this.expr.getVars();
		 //List fold
		var reducer = (accumulator, value) => Lattice.lub(accumulator,value);
		var expr_lvl = vars.reduce(reducer, false);

		let pc_lvl = sec_conf.pc[0];
		var var_lvl = sec_conf.ssto[this.stringvar];
		if(var_lvl){
			if(Lattice.leq(pc_lvl, var_lvl)){
				sec_conf.ssto[this.stringvar] = Lattice.lub(pc_lvl, expr_lvl);
			}else{
				secc_conf.error = "Illegal Assignment";
			}
		}
		else{
			sec_conf.ssto[this.stringvar] = Lattice.lub(pc_lvl, expr_lvl);
		}
		return sec_conf;

	}
	
}

module.exports = AssingLab;