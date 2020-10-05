const Lattice = require('../Lattice');
class FieldLookupLab{
	constructor(stringvar, object, field){
		this.stringvar=stringvar;
		this.object=object;
		this.field=field;
	}
	interpret(sec_conf){
		var lev_o = sec_conf.ssto.getLvl(this.object);
		var lev_f = sec_conf.ssto.getLvl(this.field);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var lev_var = sec_conf.ssto[this.stringvar];
		console.log(lev_var);
		if(lev_var == undefined){
			lev_var = lev_ctx;
		} 
		if(Lattice.leq(lev_ctx, lev_var)){
			var field = this.field;
			field = sec_conf.sheap.heap[this.object].sec_object.field;
			console.log(field);
			var lub = Lattice.lub(lev_ctx, field.val_lvl);
			sec_conf.ssto[this.stringvar] = lub;

		} else{
			sec_conf.error = "Illegal Field Lookup";
		}

		return sec_conf;
	}
}

module.exports= FieldLookupLab;
