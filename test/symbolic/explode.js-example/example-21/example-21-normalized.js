const child_process = require('child_process');
const f = function (x) {
    const v1 = child_process.execSync(x);
    v1;
};
module.exports = f;