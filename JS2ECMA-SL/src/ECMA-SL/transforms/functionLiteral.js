const {
  getLetDeclarations,
  getVarDeclarations,
  getFunctionDeclarations,
  replaceFuncDeclarations,
} = require("../utils/getDeclarations");
const { hasStrictDirective } = require("../utils/strict");

module.exports = {
  transform: function (obj) {
    if (!["FunctionExpression", "FunctionDeclaration"].includes(obj.type)) {
      throw Error(
        'Unexpected object type; Expecting "FunctionExpression" or "FunctionDeclaration"'
      );
    }

    console.log(`Function AST of ${obj.id.name} BEFORE transform`);
    console.log(obj);

    obj.params = obj.params.map((param) => param.name);

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
    obj.body.variableDeclarations = variableDeclarations;
    obj.body.functionDeclarations = functionDeclarations;
    obj.body.letDeclarations = letDeclarations; 

    obj.body.codeType = "function";

    obj.body.strict = hasStrictDirective(obj.body.body);

    console.log(`Function AST of ${obj.id.name} AFTER transform`);
    console.log(obj);


    return obj;
  },
};
