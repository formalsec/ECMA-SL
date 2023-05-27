// testing taint from parameter to eval function call
// multi scope function

module.exports = function f(x) {
    return function() { 
        return eval(x);
    }
};