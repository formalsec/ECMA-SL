// testing taint from parameter to eval function call
// testing object reconstruction

module.exports = function f(obj) {
    obj = obj || {};
    return eval(obj.prop);
}