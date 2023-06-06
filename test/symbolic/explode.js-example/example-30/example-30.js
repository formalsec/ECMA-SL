// testing taint by accessing object property

module.exports = function f(p, z) {
    const c = {};
    c[p] = z;
    let x = c["w"];
    return eval(x);
};