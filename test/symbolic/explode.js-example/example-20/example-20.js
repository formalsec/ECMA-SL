// testing taint from parameter to eval function call
// multi scope function using this

module.exports = function f(x) {
  this.input = x;

  f.prototype.ev = function() {
    let self = this;
    eval(self.input);
  }
}