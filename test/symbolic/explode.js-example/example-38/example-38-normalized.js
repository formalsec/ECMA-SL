const f = function (obj) {
    const v1 = {};
    obj = obj || v1;
    const v2 = obj.prop;
    const v3 = eval(v2);
    return v3;
};
module.exports = f;