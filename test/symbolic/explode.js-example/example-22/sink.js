// testing taint from parameter to eval function call
// source and sink in different files

module.exports = function f(y) { 
    return eval(y);
}