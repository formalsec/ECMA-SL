const exec = require('cross-spawn');
const f = function (x, y) {
    const v1 = exec(x, y);
    v1;
};
module.exports = f;