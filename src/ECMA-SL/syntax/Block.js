const Stmt = require("./Stmt");

class Block extends Stmt {
  constructor(stmtsList = []) {
    super();
    this.statements = stmtsList;
  }

  toString() {
    return `{ ${this.statements.map((stmt) => stmt.toString()).join("; ")} }`;
  }
}

module.exports = Block;
