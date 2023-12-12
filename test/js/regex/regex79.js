
var arr = /\w{3}\d?/.exec("CE\uFFFFL\uFFDDaba12");
console.log(arr[0]);
assert.sameValue(arr[0], "aba1");