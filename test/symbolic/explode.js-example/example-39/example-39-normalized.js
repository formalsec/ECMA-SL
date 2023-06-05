const f = function () {
    arg0 = arguments[0];
    arg1 = arguments[1];
    len = arguments.length;
    var x = arg1;
    let i = 0;
    let v1 = i < len;
    while (v1) {
        const v3 = x.prop;
        const v4 = arguments[i];
        const v5 = v4.prop;
        const v6 = v3 == v5;
        if (v6) {
            const v7 = arguments[i];
            const v8 = v7.prop;
            const v9 = eval(v8);
            return v9;
        }
        const v2 = i++;
        v1 = i < len;
    }
};
module.exports = f;