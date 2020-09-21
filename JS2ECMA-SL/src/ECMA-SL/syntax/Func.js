const Stmt = require("./Stmt");

class Func {
  constructor(name, params, body) {
    this.name = name;
    this.params = params;
    this.body = body;
  }

  toString() {
    return `function ${this.name} (${this.params}) ${this.body.toString()}`;
  }
}

Func.fromJSON = function (obj) { 
  var name = obj.name; 
  var params = obj.params;
  var body = Stmt.fromJSON(obj.body);  
  return new Func(name, params, body); 
}

module.exports = Func;
