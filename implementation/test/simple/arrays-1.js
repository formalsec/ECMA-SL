/**
  @return: "ten"
*/

function f_three() { return "ten"; }

function f_two() { return 5; }

function f_one() { return "one"; }

/**
  *  @id top
  */
function top(f1, f2) {
  var a = [ f_one, f_two, f_three ];
  var v1 = (a[f1])();
  var v2 = (a[f2])();
  if (v1 == v2) {
    return v1;
  } else {
    return 0;
  }
}

f_one();
f_two();
f_three();

var ret1 = top(2, "2");

Assert(ret1 = "ten")