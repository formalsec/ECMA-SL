/**
  @return 15
*/

function top(n) {
  var total = 0;
  for(var i = 0; i < n; i++) {
    total += i;
  }
  return total;
}

var ret1 = top(6);

AssertEquals(ret1, 15)
