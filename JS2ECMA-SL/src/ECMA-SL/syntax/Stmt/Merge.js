const MergeLab = require("../Labels/MergeLab");

class Merge {
  constructor() {}

  interpret(config){
  	config.cont = config.cont.slice(1);
  	return {config : config, seclabel: new MergeLab()};
  }
}

module.exports = Merge
