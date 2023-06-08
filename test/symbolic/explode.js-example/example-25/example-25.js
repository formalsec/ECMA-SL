// testing taint from parameter to exec function call

const cp = require('child_process');

module.exports = function f(x) {
    var y; 
    y = cp.execSync(x);
}