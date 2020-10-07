const Lattice = require("../Lattice");

class UpgVarLab{
	constructor(stringvar, lev){
		this.stringvar = stringvar;
		this.lev = Lattice.parseLvl(lev);
		
	}


	interpret(sec_conf){
		var pc_lvl = sec_conf.pc[0];
		var var_lvl = sec_conf.ssto.getVarLvl(this.stringvar);
		if(this.lev != undefined){
			if(Lattice.leq(pc_lvl, var_lvl)){
				sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(this.lev, pc_lvl));
				console.log("SECSTORE = "+ this.stringvar +" <-"+ Lattice.lub(this.lev, pc_lvl) );
			} else {
				sec_conf.error = "Illegal UpgVarLab: " + this.stringvar + " " +  this.lev;
			}
		} else{
			sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(this.lev, pc_lvl));
			console.log("SECSTORE = "+ this.stringvar +" <-"+ Lattice.lub(this.lev, pc_lvl) );
		}
		return sec_conf;
	}
}
module.exports = UpgVarLab;

