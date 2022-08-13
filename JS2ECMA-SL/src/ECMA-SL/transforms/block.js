const getLetDeclarations = require("../utils/getDeclarations").getLetDeclarations;

module.exports = {
    transform: function (obj) {
      if (obj.type !== "BlockStatement") {
            throw Error('Unexpected object type; Expecting "BlockStatement"');
      }
      const letDeclarations = getLetDeclarations(obj).reduce(
        // remove repeated variables
        (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
        []
      );

      obj.letDeclarations = letDeclarations;

      return obj;
    },
};