var re = /b/;

var res1 = re.exec('aab').index;
var res2 = re.exec('😃aab').index;

assert.sameValue(res1, 2);
assert.sameValue(res2, 4);
assert.sameValue("😃".length, 2);