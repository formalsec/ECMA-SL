module.exports = {
  transform: function (obj) {
    if (obj.type !== "ForInStatement") {
      throw Error('Unexpected object type; Expecting "ForInStatement"');
    }

    if (obj.left !== null && typeof obj.left === "object") {
      if (obj.left.type === "VariableDeclaration") {
        obj.left = obj.left.declarations[0];
      }
    }
    return obj;
  },
};
