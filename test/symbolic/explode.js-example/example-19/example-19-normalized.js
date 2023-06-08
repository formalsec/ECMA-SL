const f = function (x) {
    var func = new Function(x);
    const v1 = func();
    return v1;
};
module.exports = f;