function f(x) {
  var g = function (y) { return x + y; }
  var y = g(2);
  console.log(y);
  return x + 10;
}
var y = f(1);
console.log(y);
