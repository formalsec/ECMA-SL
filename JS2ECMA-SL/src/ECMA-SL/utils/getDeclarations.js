const traverse = require("./traverse");
const mapper = require("./mapper");

module.exports = {
  getVarDeclarations: getVarDeclrs,
  getFunctionDeclarations: getFuncDeclrs,
  replaceFuncDeclarations: replaceFuncDeclarations
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
        return {
          stop: true,
          data: [obj],
        };
      
      case "FunctionExpression": 
        return {
          stop: true, 
          data: [] 
        }

      default:
        return {
          stop: false,
          data: [],
        };
    }
  }
  return traverse(callback, obj).data;
}


function replaceFuncDeclarations () {
  function callback(obj) {
    if (!obj) return obj; 

    switch (obj.type) {
      case "FunctionDeclaration": obj.id;

      default: obj
    }
  }

  return mapper(callback, obj);
}
