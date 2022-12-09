/* Boundary assertions */

var r = /^a/;

var ret = r.exec("ab");

assert.sameValue(ret[0], "a");