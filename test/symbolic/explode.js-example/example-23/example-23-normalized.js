const f = function (source1, source2) {
    const Func = function () {
    };
    ;
    const v1 = Func.prototype;
    v1.x = '2';
    const myFunc = new Func();
    if (source1) {
        const v2 = myFunc.x;
        myFunc[source2] = v2 + source1;
    }
    const v3 = myFunc.x;
    const v4 = eval(v3);
    return v4;
};
module.exports = f;