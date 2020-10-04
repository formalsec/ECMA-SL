const Lattice = require("../Lattice");

class BranchLab{
	constructor(expr){
		this.expr=expr;
	}
	
	interpret(sec_conf){
		var vars = [];
		vars = this.expr.getVars();
		var reducer = (accumulator, value) => Lattice.lub(accumulator,value);
		var expr_lvl = vars.reduce(reducer, false);
		let pc_lvl = sec_conf.pc[0];
		sec_conf.pc = [Lattice.lub(pc_lvl,expr_lvl)].concat(sec_conf.pc);

		return sec_conf;
	}
	
}
module.exports= BranchLab;