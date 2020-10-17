/**
  @return: 22
*/

var total = 0;

var o = {
  fib: function f_fib(x) {
    if (x < 3) {
        return 1;
    } else {
        return this.fib(x-1) + this.fib(x-2);
    }
  },

  fact: function f_fact(x) {
    if (x < 1) {
        return 1;
    } else {
        return this.fact(x-1) * x;
    }
  },

  sum: function f_sum(x) {
    if (x < 1) {
        return 0;
    } else {
        return this.sum(x-1) + x;
    }
  }
};

total += o[ "fib" ](7);
total += o[ "fact" ](3);
total += o[ "sum" ](2);

total;

Assert(total = 22)