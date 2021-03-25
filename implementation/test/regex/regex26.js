/* Boundary assertions */

var r = /^a$/;

var ret = r.exec("a\nb");

assert.sameValue(ret, null);