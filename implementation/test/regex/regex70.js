var str = 'She sells seashells by the seashore.';
var re = /sh/g;

var ret = str.replace(re, "$$" + 'sch');

console.log(ret);

assert.sameValue(ret, 'She sells sea$schells by the sea$schore.');