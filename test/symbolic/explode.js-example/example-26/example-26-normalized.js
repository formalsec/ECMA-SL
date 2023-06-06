const child_process = require('child_process');
const f = function (x) {
    cmd = [
        'cat',
        '-n'
    ];
    const v1 = cmd.push(x);
    v1;
    const v2 = cmd.join(' ');
    const v3 = child_process.execSync(v2);
    v3;
};
module.exports = f;