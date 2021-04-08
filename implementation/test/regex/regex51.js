var r = /[^a-c]+/;

var ret = r.exec("abcdef");
assert.sameValue(ret[0], "def");

ret = r.exec("abcabc");
assert.sameValue(ret, null);