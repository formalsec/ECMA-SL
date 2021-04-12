var r = /\d\D/;

var ret = r.exec("0a");
assert.sameValue(ret[0], "0a");

ret = r.exec("\t a\t");
assert.sameValue(ret, null);