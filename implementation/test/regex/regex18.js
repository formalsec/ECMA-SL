/* Positive look-ahead */

var r = /a*(?!aa)/;

var ret = r.exec("aaaaa");

assert.sameValue(ret[0], "aaaaa");