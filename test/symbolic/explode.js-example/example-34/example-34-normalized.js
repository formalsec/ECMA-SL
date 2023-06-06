var child = require('child_process');
const f1 = function (x) {
    const cmd = x.cmd;
    const f2 = function (y) {
        const v1 = child.execSync(y);
        v1;
    };
    const v2 = f2(cmd);
    v2;
};
module.exports = f1;