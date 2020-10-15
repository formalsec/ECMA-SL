/**
  @return 1
*/

function top(f, n) {
  var o = { one: 3, two: 5, three: 7 };
  if (o[f] == n) {
    return 1;
  } else {
    return 0;
  }
}

var ret1 = top("two", 5);

Assert (ret1 = 1)