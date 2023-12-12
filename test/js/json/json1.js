
var n = JSON.parse('1e3');
assert.sameValue(n, 1000);

var n = JSON.parse('1.0e3');
assert.sameValue(n, 1000);

var n = JSON.parse('1.0E3');
assert.sameValue(n, 1000);

var n = JSON.parse('1.0E+3');
assert.sameValue(n, 1000);

n = JSON.parse('1e-3');
assert.sameValue(n, 0.001);