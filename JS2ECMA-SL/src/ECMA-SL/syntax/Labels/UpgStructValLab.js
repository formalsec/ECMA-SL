const Lattice = require("../Lattice");

class UpgStructValLab{  // Object_lvl
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
			if(Lattice.leq(lev_ctx, sec_conf.sheap.getObjectLvl(this.location))){
				sec_conf.sheap.setObjectLvl(Lattice.lub(this.lvl, lev_ctx));
			} else{
				sec_conf.error = "Illegal P_Val Upgrade";
			}
		} 
		return sec_conf;
	}
}

module.exports = 'UpgStructValLab';

/*
 | UpgStructValLab (loc, e_o, lvl) -> (*UpgObjLab <- mudar*)
    let lev_o = expr_lvl ssto e_o in
    let lev_ctx = SL.lubn [lev_o;(check_pc pc)] in
    (match SecHeap.get_val sheap loc with
     | Some lev ->
       if SL.leq lev_ctx lev then (
         SecHeap.upg_struct_val sheap loc (SL.lub lvl lev_ctx);
         MReturn (scs, sheap, ssto, pc))
       else
         MFail((scs,sheap,ssto,pc), "Illegal P_Val Upgrade")
     | None -> raise (Except "Internal Error"))

 */