var str = 'uid=31';
var re = /(uid=)(\d+)/;

var ret = str.replace(re, "$11" + 15);

console.log(ret);

assert.sameValue(ret, 'uid=115');