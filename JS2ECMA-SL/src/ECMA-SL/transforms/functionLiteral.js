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
    if (!["FunctionExpression", "FunctionDeclaration", "ArrowFunctionExpression"].includes(obj.type)) {
      throw Error(
        'Unexpected object type; Expecting "FunctionExpression", "FunctionDeclaration" or "ArrowFunctionExpression"'
      );
    }

    const paramsInitializers = []

    paramsNames = obj.params.map((param) => {
      switch (param.type) {
        case "Identifier": {
          paramsInitializers.push({name: param.name, initializer: null, rest: false});
          return param.name;
        }
        case "AssignmentPattern": {
          paramsInitializers.push({name: param.left.name, initializer: param, rest: false});
          return param.left.name;
        }
        case "RestElement": {
          paramsInitializers.push({name: param.argument.name, initializer: null, rest: true});
          return param.argument.name;
        }
      }
    });

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
    obj.body.paramsInitializers = paramsInitializers;

    obj.params = paramsNames;

    obj.body.codeType = "function";

    obj.body.strict = hasStrictDirective(obj.body.body);

    return obj;
  },
};
