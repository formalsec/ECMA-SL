// Hexadecimal

var str = '\x41';
var str2 = 'A';

assert.sameValue(str, str2);

var re = /\x41/;
var ret = re.test("A");
assert.sameValue(ret, true);

re = /[\x41]/;
ret = re.test("A");
assert.sameValue(ret, true);

assert.sameValue(String.fromCharCode(0x0041), "A");

str = "\xe0\xa4\xb1";
str2 = "à¤±";
assert.sameValue(str, str2);

re = /[\xe0\xa4\xb1]/;
ret = re.test("à¤±");
assert.sameValue(ret, true);