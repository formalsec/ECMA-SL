const EmptyLab = require("../Labels/EmptyLab");
class Skip {
  constructor() {}

  interpret(config){
  	return {config : config, seclabel: new EmptyLab()};
  }
}

module.exports = Skip
