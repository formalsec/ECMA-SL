/**
  @return 8
*/

function top(x) {
  if (x < 3) {
    return 1;
  } else {
    return top(x-1) + top(x-2);
  }
}

var ret1 = top(6);

AssertEquals(ret1, 8)
