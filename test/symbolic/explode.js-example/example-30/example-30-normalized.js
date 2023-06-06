const f = function (p, z) {
    const c = {};
    c[p] = z;
    let x = c['w'];
    const v1 = eval(x);
    return v1;
};
module.exports = f;