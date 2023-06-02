// testing taint from parameter to eval function call

module.exports = function f(x) {
    x.y = eval("(" + x.y + ")");
    return x.y;
}