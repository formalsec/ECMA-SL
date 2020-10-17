/**
  @return: 12
**/

function top(s) {
  var total = 0;
  for(var i = 0; i < s; i++) {
    try {
      if (i % 2 == 1) {
        throw "odd number";
      }
      total += i;
    } catch (e) {
    }
  }
  return total;
}

var ret1 = top(7);

Assert(ret1 = 12)