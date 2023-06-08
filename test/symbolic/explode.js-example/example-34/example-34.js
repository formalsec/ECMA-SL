var child = require('child_process');

module.exports = function f1(x) {
    const cmd = x.cmd;
    function f2(y) {
        child.execSync(y)
    }
    f2(cmd);
}