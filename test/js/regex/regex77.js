var A = 0x0041;
assert.sameValue(A, 65);

var str = String.fromCharCode(A);
assert.sameValue(str, "A");

var re = /\cA/;
var ret = re.exec(String.fromCharCode(1));

var re = /[\cA]/;
var ret = re.exec(String.fromCharCode(1));
