const {
  getVarDeclarations,
  getFunctionDeclarations,
  replaceFuncDeclarations,
} = require("../utils/getDeclarations");
const { hasStrictDirective } = require("../utils/strict");

module.exports = {
  transform: function (obj) {
    if (obj.type !== "Program") {
      throw Error('Unexpected object type; Expecting "Program"');
    }

    const variableDeclarations = getVarDeclarations(obj).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );
    const functionDeclarations = getFunctionDeclarations(obj);

    obj.body = obj.body.map(replaceFuncDeclarations);
    obj.variableDeclarations = variableDeclarations;
    obj.functionDeclarations = functionDeclarations;

    obj.codeType = "global";

    obj.strict = hasStrictDirective(obj.body);

    return obj;
  },
};
