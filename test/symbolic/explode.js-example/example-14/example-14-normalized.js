const f = function (x) {
    try {
        const v1 = eval(x);
        return v1;
    } catch (e) {
        const v2 = console.log(e);
        v2;
    }
};
module.exports = f;