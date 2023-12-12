var r = new RegExp("a|b");

var ret = r.exec("a");
console.log(ret);
assert.sameValue(ret[0], "a");
assert.sameValue(ret.index, 0);
