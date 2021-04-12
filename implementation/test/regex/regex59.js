var ret = "abc".search(/b/ig);

assert.sameValue(ret, 1);

ret = "aaa".search(/b/ig);

assert.sameValue(ret, -1);

var str = "Mr. Blue has a blue house"
ret = str.search("blue");
assert.sameValue(ret, 15);