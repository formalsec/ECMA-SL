// testing taint from parameter to eval function call
// multi scope function using this

module.exports = function f(x) {
    this.input = x;
    return function() {
        return eval(this.input);    
    }
}