var str = "easy\bto\u0008ride";

var arr = /[^\b]+/g.exec(str);
assert.sameValue(arr[0], "easy");

str = "easy\u0008to\bride";
arr = /[^\b]+/g.exec(str);
assert.sameValue(arr[0], "easy");