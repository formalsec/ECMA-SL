// testing taint from parameter to eval function call
// conditional sentence

module.exports = function f(x) {
    const evalArgs = true ? x : x.split(' ');
    return eval(evalArgs);  
}