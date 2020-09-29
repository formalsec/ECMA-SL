const traverse = require("./traverse");

module.exports = {
  getVarDeclarations: getVarDeclrs,
  getFunctionDeclarations: getFuncDeclrs,
};

function getVarDeclrs(obj) {
  function callback(obj) {
    if (!obj) {
      return {
        stop: true,
        data: [],
      };
    }

    switch (obj.type) {
      case "FunctionDeclaration":
      case "FunctionExpression":
        return {
          stop: true,
          data: [],
        };

      case "VariableDeclaration":
        const vars = obj.declarations.reduce(
          (acc, declr) => acc.concat(declr.id.name),
          []
        );

        return {
          stop: true,
          data: vars,
        };

      default:
        return {
          stop: false,
          data: [],
        };
    }
  }

  return traverse(callback, obj).data;
}

function getFuncDeclrs(obj) {
  function callback(obj) {
    if (!obj) {
      return {
        stop: true,
        data: [],
      };
    }

    switch (obj.type) {
      case "FunctionDeclaration":
      case "FunctionExpression":
        return {
          stop: true,
          data: [obj],
        };

        if (obj.id) {
          retObj.data = [obj.id.name];
        }

        return retObj;

      default:
        return {
          stop: false,
          data: [],
        };
    }
  }
  return traverse(callback, obj).data;
}
