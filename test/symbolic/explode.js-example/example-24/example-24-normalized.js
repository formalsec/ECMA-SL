const v1 = require('child_process');
const execSync = v1.execSync;
const f = function (x) {
    const v2 = execSync(x);
    v2;
};
module.exports = f;