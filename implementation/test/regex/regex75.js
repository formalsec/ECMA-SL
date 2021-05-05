// Octal
var str = '\101';
var str2 = 'A';

assert.sameValue(str, str2);

var re = /\101/;

var ret = re.test("A");

assert.sameValue(ret, true);

re = /[\101]/;

ret = re.test("A");

assert.sameValue(ret, true);