// testing taint from parameter to eval function call
// "new Function" instead of "Function"

module.exports = function f(x) {
    var func = new Function(x);
    return func();
}