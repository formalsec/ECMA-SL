/**
  @return "aaaaaa"
*/

function top(x, y) {
  var str = "";
  for(var z = 0; z != x; z++) {
    str = y + str;
  }
  return str;
}

var ret1 = top(3, "aa");

Assert(ret1 = "aaaaaa")