const cp = require('child_process');
const f = function (x) {
    var y;
    y = cp.execSync(x);
};
module.exports = f;