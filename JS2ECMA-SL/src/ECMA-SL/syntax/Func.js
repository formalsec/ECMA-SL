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

module.exports = Func;
