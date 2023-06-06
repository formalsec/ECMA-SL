const f = function (obj1, obj2) {
    obj1 = obj1 || obj2;
    const v1 = obj1.prop;
    const v2 = eval(v1);
    return v2;
};
module.exports = f;