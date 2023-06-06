// testing taint from parameter to eval function call

const exec = require('cross-spawn');

module.exports = function f(x, y) {
    exec(x, y);
}