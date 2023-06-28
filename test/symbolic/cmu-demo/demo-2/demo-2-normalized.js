const f = function (obj, malicious) {
    const v1 = obj.inputs;
    v1.x = malicious;
    const v2 = obj.conds;
    if (v2) {
        const v3 = obj.conds;
        const v4 = v3.cond1;
        const v5 = v4 * 10;
        const v6 = v5 >= 100;
        if (v6) {
            const v7 = obj.inputs;
            const v8 = v7.x;
            const v9 = eval(v8);
            return v9;
        }
    }
};
module.exports = f;