/* Boundary assertion  */

var r = /^a/;

var ret = r.exec("b\na");

assert.sameValue(ret, null);