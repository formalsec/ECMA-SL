// testing taint from parameter to eval function call

module.exports = function f(x) {
    return eval(x);
};