module.exports = function f(o) {
    if (o.z > 0) {
        o.y = "2";
        return eval(o.y + o.w);
    } else if (o.z.cond1) {
        return eval(o.x)
    }
};