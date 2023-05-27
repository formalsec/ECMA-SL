// testing taint from parameter to eval function call
// sink argument is a template string

module.exports = function f(x) {
    return eval(`${x}`);
}
