const {
  getVarDeclarations,
  getFunctionDeclarations,
  replaceFuncDeclarations,
} = require("../utils/getDeclarations");

module.exports = {
  transform: function (obj) {
    if (!["FunctionExpression", "FunctionDeclaration"].includes(obj.type)) {
      throw Error(
        'Unexpected object type; Expecting "FunctionExpression" or "FunctionDeclaration"'
      );
    }

    obj.params = obj.params.map((param) => param.name);

    const variableDeclarations = getVarDeclarations(obj.body).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );
    const functionDeclarations = getFunctionDeclarations(obj.body);

    obj.body = replaceFuncDeclarations(obj.body);
    obj.body.variableDeclarations = variableDeclarations;
    obj.body.functionDeclarations = functionDeclarations;

    obj.body.codeType = "function";

    return obj;
  },
};
