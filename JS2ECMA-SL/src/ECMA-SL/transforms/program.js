const {
  getVarDeclarations,
  getFunctionDeclarations,
} = require("../utils/getDeclarations");

module.exports = {
  transform: function (obj) {
    if (obj.type !== "Program") {
      throw Error('Unexpected object type; Expecting "Program"');
    }

    obj.globalVars = getVarDeclarations(obj).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );
    obj.globalFuncs = getFunctionDeclarations(obj).reduce(
      // remove repeated functions
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    return obj;
  },
};
