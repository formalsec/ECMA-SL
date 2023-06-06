// testing taint from parameter to exec function call
// child_process.exec() instead of just exec

const execSync = require('child_process').execSync;

module.exports = function f(x) {
    execSync(x);
}