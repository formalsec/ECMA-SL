var child = require('child_process');

module.exports = function f(x) {
  const arr = x.join(' ')
  child.execSync(arr)
}