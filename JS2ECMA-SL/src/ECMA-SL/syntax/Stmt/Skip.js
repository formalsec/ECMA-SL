const EmptyLab = require("../Labels/EmptyLab");
class Skip {
  constructor() {}

  interpret(config){
  	console.log(">SKIP");
  	return {config : config, seclabel: new EmptyLab()};
  }
}

module.exports = Skip
