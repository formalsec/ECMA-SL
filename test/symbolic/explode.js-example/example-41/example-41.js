// testing taint from parameter to exec function call
// testing object reconstruction

module.exports = function f(obj) {
    if(!obj) {
        obj = {};
        obj.prop = "2+2";
    }
    return eval(obj.prop);
}