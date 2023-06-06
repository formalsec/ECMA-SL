const f = function (x) {
    let evalArgs;
    const v1 = x.split(' ');
    if (true) {
        evalArgs = x;
    } else {
        evalArgs = v1;
    }
    const v2 = eval(evalArgs);
    return v2;
};
module.exports = f;