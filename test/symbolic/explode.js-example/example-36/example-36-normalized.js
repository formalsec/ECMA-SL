var child = require('child_process');
const f1 = function (x) {
    const so = x.so;
    const f2 = function (y) {
        const cmd = y.cmd;
        const v1 = child.execSync(cmd);
        v1;
    };
    const v2 = f2(so);
    v2;
};
module.exports = f1;