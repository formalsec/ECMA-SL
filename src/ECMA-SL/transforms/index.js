const Switch = require("./switch");
const PropertyAccessors = require("./propertyAccessors");

module.exports = {
  transformObject: function (obj) {
    if (obj.type === "SwitchStatement") {
      return Switch.transform(obj);
    }
    if (obj.type === "MemberExpression") {
      return PropertyAccessors.transform(obj);
    }
    return obj;
  },
};
