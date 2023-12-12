/* Repeat matcher */

var r = /c{3,}/;

var ret = r.exec("ccaaaaacc");

assert.sameValue(ret, null);