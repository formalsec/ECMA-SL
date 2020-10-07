const SCSFrame = require('../../SCSFrame');
const Store = require('../../SecStore');

class AssignCallLab{
	constructor(stringvar, f, args){
		this.stringvar = stringvar;
		this.f = f;
		this.args = args;
	}
	interpret(sec_conf){
		sec_conf.scs.push(new SCSFrame(sec_conf.pc, sec_conf.ssto, this.stringvar));
		var lvls = this.args.map(e => sec_conf.ssto.getExprLvl(e));
      if(this.f != undefined){
       	sec_conf.ssto = new SecStore(this.f.params, lvls);
      }
		return sec_conf;
	}
}
module.exports = AssignCallLab;