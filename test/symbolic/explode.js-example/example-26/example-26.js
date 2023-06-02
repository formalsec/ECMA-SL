// testing taint from parameter to exec function call
// tainted paremeter is pushed into an array and given as sink argument

const child_process = require('child_process');

module.exports = function f(x) {
    cmd = ["cat", "-n"];
    cmd.push(x);
    child_process.execSync(cmd.join(' '));
}