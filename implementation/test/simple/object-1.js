/**
  @return 9
*/

function top(base) {
  this.base = base;
  this.fib = function fib(x) {
    if (x < 3) {
        return 1;
    } else {
        return this.fib(x-1) + this.fib(x-2);
    };
  }
  this.silly = function silly(x) {
    return this.fib(x) + this.base;
  }
  return this;
}

var x = new top(1);
var ret1 = x.silly(6);

Assert(ret1 = 9)