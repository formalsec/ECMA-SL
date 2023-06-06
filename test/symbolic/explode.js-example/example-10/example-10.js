// testing taint by accessing object property

module.exports = function f(c, p) {
    let x = c[p];
    return eval(x);
};