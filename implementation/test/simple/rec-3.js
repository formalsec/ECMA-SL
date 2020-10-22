/**
  @return 8
*/

var o = {
  fib: function f_fib(x) {
    if (x < 3) {
      return 1;
    } else {
      return this.fib(x-1) + this.fib(x-2);
    }
  },

  top: function f_top(f, x) {
    return this[f](x);
  }

};

var ret1 = o.top("fib", 6);

AssertEquals(ret1,8)
