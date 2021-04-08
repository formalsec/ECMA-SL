var r = /\s{2}\S?/;

var ret = r.exec("\t 0");
assert.sameValue(ret[0], "\t 0");

ret = r.exec("\t a\t");
assert.sameValue(ret[0], "\t a");

ret = r.exec("\t ");
assert.sameValue(ret[0], "\t ");

ret = r.exec("  ");
assert.sameValue(ret[0], "  ");

ret = r.exec(" a");
assert.sameValue(ret, null);