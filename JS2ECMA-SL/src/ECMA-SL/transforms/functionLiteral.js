const {
  getLetDeclarations,
  getVarDeclarations,
  getFunctionDeclarations,
  replaceFuncDeclarations,
} = require("../utils/getDeclarations");
const { isSimpleParameterList } = require("../utils/isSimpleParameterList");
const { containsExpressions } = require("../utils/containsExpression");
const { hasStrictDirective } = require("../utils/strict");

module.exports = {
  transform: function (obj) {
    if (!["FunctionExpression", "FunctionDeclaration"].includes(obj.type)) {
      throw Error(
        'Unexpected object type; Expecting "FunctionExpression" or "FunctionDeclaration"'
      );
    }



    paramsNames = obj.params.map((param) => param.name);

    const variableDeclarations = getVarDeclarations(obj.body).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    const letDeclarations = getLetDeclarations(obj.body).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );


    const functionDeclarations = getFunctionDeclarations(obj.body);

    obj.body = replaceFuncDeclarations(obj.body);

    obj.body.isSimpleParameterList = isSimpleParameterList(obj.params);
    obj.body.variableDeclarations = variableDeclarations;
    obj.body.functionDeclarations = functionDeclarations;
    obj.body.letDeclarations = letDeclarations;
    obj.body.containsExpression = containsExpressions(obj.params);

    obj.params = paramsNames;

    obj.body.codeType = "function";

    obj.body.strict = hasStrictDirective(obj.body.body);


    return obj;
  },
};
