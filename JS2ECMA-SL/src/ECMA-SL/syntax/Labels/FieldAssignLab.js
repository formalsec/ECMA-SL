const Lattice = require('../Lattice');

class FieldAssignLab{
	constructor(object, field, expression){
		this.object = object;
		this.field = field;
		this.expression = expression;
	}

	interpret(sec_conf){
		var lev_o = sec_conf.ssto.getLvl(this.object);
		var lev_f = sec_conf.ssto.getLvl(this.field);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var lev_expr = sec_conf.ssto.getLvl(this.expression);
		console.log("Obj: "+this.object);
		console.log("Field: "+this.field);
		console.log(sec_conf.sheap.heap[this.object].sec_object);
		var field = this.field;
		field = sec_conf.sheap.heap[this.object].sec_object.field;

		if(field){
			if(Lattice.leq(lev_ctx,field.val_lvl)){
				sec_conf.sheap.heap[this.object].sec_object.field.val_lvl = Lattice.lub(lev_expr, lev_ctx);
			} else{
				sec_conf.error = "Illegal Field Assign";
			}
		} else{
			var struct_lvl = sec_conf.sheap.heap[this.object].struct_lvl;
			if(struct_lvl != undefined){
				if(Lattice.leq(lev_ctx, struct_lvl)){
					sec_conf.sheap.heap[this.object].sec_object.field = {};
					sec_conf.sheap.heap[this.object].sec_object.field.exists_lvl = lev_ctx;
					sec_conf.sheap.heap[this.object].sec_object.field.val_lvl = (Lattice.lub(lev_expr, lev_ctx));
				}
			}
			else{
				throw Error("Internal Error");
			}
		}

		return sec_conf;
	}

}

module.exports= FieldAssignLab;