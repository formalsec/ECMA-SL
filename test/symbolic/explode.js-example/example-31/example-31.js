// Thesis example

module.exports = function f(obj, malicious) {
  obj.inputs.x = malicious;
  if (obj.conds) {
    if (obj.conds.cond1 * 10 >= 100) {
      return eval(obj.inputs.x);
    } else {
      return obj.inputs.y;
    }
  } else {
    return obj.z;
  }
}