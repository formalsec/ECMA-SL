/**
  @return 8
*/

var o = {
  top: function f_top(x) {
    if (x < 3) {
      return 1;
    } else {
      return this.top(x-1) + this.top(x-2);
    }
  }
};

var ret1 = o.top(6);

AssertEquals(ret1, 8)
