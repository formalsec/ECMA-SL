/* Boundary assertions */

var r = /^a/m;

var ret = r.exec("ab");

assert.sameValue(ret[0], "a");