class ReturnLab{
	constructor(expr){
		this.expr=expr;
	}
	interpret(sec_conf){
		var frame = sec_conf.scs.pop();
		var vars = this.expr.getVars();
  		var return_lvl = sec_conf.ssto.getLvl(vars);
  		sec_conf.ssto = frame.ssto;
  		sec_conf.pc = frame.pc;
  		sec_conf.ssto.sto[frame.stringVar]=return_lvl;
		return sec_conf;
	}
}

module.exports= ReturnLab;