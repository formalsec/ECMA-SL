var r = /ab/;
var ret = r.test("ccabcc");

assert.sameValue(ret, true);