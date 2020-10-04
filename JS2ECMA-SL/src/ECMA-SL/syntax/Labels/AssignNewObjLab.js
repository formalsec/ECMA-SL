class NewLab{
	constructor(stringvar, location){
		this.stringvar = stringvar;
		this.location = location;
	}

	interpret(sec_conf){
		var pc_lvl = sec_conf.pc[0];
		sec_conf.sheap.heap[this.location] = {sec_object: {}, struct_lvl : pc_lvl, obj_lvl : pc_lvl};
		sec_conf.ssto.sto[this.stringvar] = pc_lvl;
		return sec_conf;
	}
}

module.exports= NewLab;