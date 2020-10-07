class ReturnLab{
	constructor(expr){
		this.expr=expr;
	}
	interpret(sec_conf){
		var frame = sec_conf.scs.pop();
  		var return_lvl = sec_conf.ssto.getExprLvl(this.expr);
  		sec_conf.ssto = frame.ssto;
  		sec_conf.pc = frame.pc;
  		sec_conf.ssto.setVarLvl(frame.stringVar, return_lvl);
		return sec_conf;
	}
}

module.exports= ReturnLab;