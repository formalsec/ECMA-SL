/**
  @return "abczbc"
*/


var f = function (x) {
  var y = x + "b";
  var z = y + "c";
  return z;
};

var a = f("a");
var b = f("z");

Assert((a ++ b) = "abczbc")