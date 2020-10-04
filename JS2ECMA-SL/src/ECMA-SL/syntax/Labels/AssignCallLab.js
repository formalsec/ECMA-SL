const SCSFrame = require('../../SCSFrame');
const Store = require('../../Store');

class AssignCallLab{
	constructor(stringvar, f, args){
		this.stringvar = stringvar;
		this.f = f;
		this.args = args;
	}
	interpret(sec_conf){
		sec_conf.scs.push(new SCSFrame(sec_conf.pc, sec_conf.ssto, this.stringvar));
		console.log(this.args);
		var vars = this.args.map(e => e.getVars());
		var lvls = sec_conf.ssto.getLvls(vars);
      if(this.f){
       	sec_conf.ssto = new Store(this.f.params, lvls);
      }
		return sec_conf;
	}
}
module.exports = AssignCallLab;