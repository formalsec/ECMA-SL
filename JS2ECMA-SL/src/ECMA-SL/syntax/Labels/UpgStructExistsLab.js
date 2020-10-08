const Lattice = require("../Lattice");

class UpgStructExistsLab{  // Struct_lvl

	constructor(location, e_o, lvl){
		this.location = location;
		this.e_o=e_o;
		this.lvl=lvl;
	}

	interpret(sec_conf){
		let lev_o = this.e_o.interpret(config.store).value;
		let lev_ctx = Lattice.lub(lev_o, sec_conf.pc[0]);
		var exists = sec_conf.sheap.locationCheck(this.location);
		if(exists){
			if(Lattice.leq(lev_ctx, sec_conf.sheap.getStructLvl(this.location))){
				sec_conf.sheap.setStructLvl(Lattice.lub(this.lvl, lev_ctx);
			} else{
				sec_conf.error = "Illegal P_Val Upgrade";
			}
		} 
		return sec_conf;
	}
}

module.exports = 'UpgStructExistsLab';