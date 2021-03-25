/* Boundary assertions */

var r = /^a$/m;

var ret = r.exec("c\na\nc");

assert.sameValue(ret[0], "a");