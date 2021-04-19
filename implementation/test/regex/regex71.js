var str = 'abcdefcdef';
var re = /c/g;

var ret = str.replace(re, "$`" + 'SS');

console.log(ret);

assert.sameValue(ret, 'ababSSdefabcdefSSdef');