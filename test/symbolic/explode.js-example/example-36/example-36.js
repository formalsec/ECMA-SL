var child = require('child_process');

module.exports = function f1(x) {
    const so = x.so;
    function f2(y) {
        const cmd = y.cmd;
        child.execSync(cmd)
    }
    f2(so);
}