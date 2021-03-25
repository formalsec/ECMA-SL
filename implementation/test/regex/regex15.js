/* Positive look-ahead */

var r = /aaa(?=aa)/;

var ret = r.exec("aaa");

assert.sameValue(ret, null);