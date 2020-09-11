const Switch = require("./switch");
const PropertyAccessors = require("./propertyAccessors");
const Assignment = require("./assignment");

module.exports = {
  transformObject: function (obj) {
    if (obj.type === "SwitchStatement") {
      return Switch.transform(obj);
    }
    if (obj.type === "MemberExpression") {
      return PropertyAccessors.transform(obj);
    }
    if (obj.type === "AssignmentExpression") {
      return Assignment.transform(obj);
    }
    return obj;
  },
};
