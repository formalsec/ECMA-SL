var r = /abd/;
var ret = r.test("ccabcc");

assert.sameValue(ret, false);