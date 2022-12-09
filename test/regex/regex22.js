/* Boundary assertions */

var r = /^a/m;

var ret = r.exec("cc\nabcc");

assert.sameValue(ret[0], "a");