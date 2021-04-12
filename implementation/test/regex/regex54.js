var r = new RegExp("\\n+", "gim");

var ret = r.exec("\n\n");
console.log(ret);
assert.sameValue(ret[0], "\n\n");
assert.sameValue(ret.index, 0);
