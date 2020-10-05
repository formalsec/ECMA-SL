const Lattice = require('../Lattice');

class FieldDeleteLab{
	constructor(object, field){
		this.object=object;
		this.field=field; 
	}
	interpret(sec_conf){
		var lev_o = sec_conf.ssto.getLvl(this.object);
		var lev_f = sec_conf.ssto.getLvl(this.field);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var field = this.field;
		field = sec_conf.sheap.heap[this.object].sec_object.field;
		if (field != undefined){
			if(Lattice.leq(lev_ctx, field.exists_lvl)){
				sec_conf.sheap.deleteField(this.object, this.field);
			}
		} else {
			sec_conf.error = "Illegal Field Delete";
		}
		return sec_conf;
	}

}

module.exports= FieldDeleteLab;
