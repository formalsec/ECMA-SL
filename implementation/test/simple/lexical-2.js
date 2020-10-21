/**
  @return "ababab"
*/

var y = "ab";

function top(z) {
  function inner(x) {
    var str = "";
    for(var z = 0; z != x; z++) {
        str = y + str;
    }
    return str;
  }
  return inner(z);
}

var ret1 = top(3);

AssertEquals(ret1, "ababab")
