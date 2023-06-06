const f = function (obj) {
    const v1 = !obj;
    if (v1) {
        obj = {};
        obj.prop = '2+2';
    }
    const v2 = obj.prop;
    const v3 = eval(v2);
    return v3;
};
module.exports = f;