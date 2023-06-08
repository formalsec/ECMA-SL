// retval = 1
function test_noarg() { return 1; }
var retval_1 = test_noarg();


// retval = 20
function test_onearg(x) { return x; }
var retval_2 = test_onearg(20);


// retval = 300
function test_ret_middle(x) {
  return x;
  return x + 2;
}
var retval_3 = test_ret_middle(300);


// retval = 4000
function test_vdecl(x) {
  var y = 4;
  return x * y;
}
var retval_4 = test_vdecl(1000);



// retval = 50000
global = 5;
function test_scope_resolution(x) {
  return x * global;
}
var retval_5 = test_scope_resolution(10000);



// retval = 600000
function test_internal_func(x) {
  var z = x;
  function internal_func() {
    return 6 * z;
  }
  return internal_func;
}
var func = test_internal_func(100000);
var retval_6 = func();


// retval = 7000000
function test_return_in_loop(x) {
  while (true) {
    return 7 * x;
  }

  return 0;
}
var retval_7 = test_return_in_loop(1000000);



// retval = 87654321
retval_1 + retval_2 + retval_3 + retval_4 + retval_5 + retval_6 + retval_7