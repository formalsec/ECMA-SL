const EmptyLab = require("../Labels/EmptyLab");
class Skip {
  constructor() {}

  toString(){
  	return "skip";
  }
  toJS(){
  	return {
      "type": "ExpressionStatement",
      "expression": {
        "type": "Literal",
        "value": 0,
        "raw": "0"
      }
    }
  }

  interpret(config){
  	console.log(">SKIP");
  	return {config : config, seclabel: new EmptyLab()};
  }
}

module.exports = Skip
