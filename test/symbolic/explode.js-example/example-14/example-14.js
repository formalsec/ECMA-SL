// testing taint from parameter to eval function call
// try catch

module.exports = function f(x) {
    try {
        return eval(x);
    } catch(e) {
        console.log(e);
    }
};