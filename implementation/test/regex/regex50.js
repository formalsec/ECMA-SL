var r = /\w+\W+/;

var ret = r.exec("0azAZ_.,-");
assert.sameValue(ret[0], "0azAZ_.,-");

ret = r.exec("0azAZ");
assert.sameValue(ret, null);