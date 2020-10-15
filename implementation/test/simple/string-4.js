/**
  @return 1
*/

function f_three() { return this.z; }

function f_two() { return this.y; }

function f_one() { return this.x; }

function top(f, n) {
  var o = { one: f_one, two: f_two, three: f_three, x: 0, y: 5, z: 10 };
  if (o[f]() == n) {
    return 1;
  } else {
    return 0;
  }
}

top("one", 0);
top("two", 1);
var ret1 = top("three", 10);

Assert (ret1 = 1)