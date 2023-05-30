// testing taint from parameter to exec function call
// child_process.exec() instead of just exec

const child_process = require('child_process');

module.exports = function f(x) {
    child_process.execSync(x);
}