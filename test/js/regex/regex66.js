var re = /hh/g;
var str = "hh oo hh";

var ret = re.exec(str);
assert.sameValue(ret.index, 0);
ret = re.exec(str);
assert.sameValue(ret.index, 6);