const f = function (x) {
    const v1 = `${ x }`;
    const v2 = eval(v1);
    return v2;
};
module.exports = f;