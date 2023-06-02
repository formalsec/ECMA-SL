const f = function (o) {
    const v1 = o.z;
    const v2 = v1 > 0;
    if (v2) {
        o.y = '2';
        const v3 = o.y;
        const v4 = o.w;
        const v5 = v3 + v4;
        const v6 = eval(v5);
        return v6;
    } else {
        const v7 = o.z;
        const v8 = v7.cond1;
        if (v8) {
            const v9 = o.x;
            const v10 = eval(v9);
            return v10;
        }
    }
};
module.exports = f;