const f = function (x) {
    const v1 = x.y;
    const v2 = '(' + v1;
    const v3 = v2 + ')';
    const v4 = eval(v3);
    x.y = v4;
    const v5 = x.y;
    return v5;
};
module.exports = f;