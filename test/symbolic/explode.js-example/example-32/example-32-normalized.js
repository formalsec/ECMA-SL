var child = require('child_process');
const f = function (x) {
    const arr = x.join(' ');
    const v1 = child.execSync(arr);
    v1;
};
module.exports = f;