// Unicode
var str = '\u0041';
var str2 = 'A';
assert.sameValue(str, str2);

var re = /\u0041/;
var ret = re.test("A");
assert.sameValue(ret, true);

re = /[\u0041]/;
ret = re.test("A");
assert.sameValue(ret, true);

str = '\u0931';
str2 = "ऱ";
assert.sameValue(str, str2);

re = /[\u0931]/;
ret = re.test("ऱ");
assert.sameValue(ret, true);
