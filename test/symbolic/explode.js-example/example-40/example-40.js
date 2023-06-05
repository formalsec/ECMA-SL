// testing taint from parameter to exec function call
// testing object reconstruction

module.exports = function f(obj1, obj2) {
    obj1 = obj1 || obj2;
    return eval(obj1.prop);
}