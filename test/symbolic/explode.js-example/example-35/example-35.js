var child = require('child_process');

module.exports = function f1(arr) {
    arr.forEach(element => { child.execSync(element)})
}