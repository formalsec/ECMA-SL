var child = require('child_process');
const f1 = function (arr) {
    const v2 = element => {
        const v1 = child.execSync(element);
        v1;
    };
    const v3 = arr.forEach(v2);
    v3;
};
module.exports = f1;