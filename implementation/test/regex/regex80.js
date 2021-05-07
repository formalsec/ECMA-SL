var str = "easy\bto\u0008ride";
console.log(str);

var arr = /[^\b]+/g.exec(str);
console.log(arr);
assert.sameValue(arr[0], "easy");