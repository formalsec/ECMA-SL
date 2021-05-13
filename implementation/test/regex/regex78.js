assert.sameValue("\xFF", "\u00FF");

var arr = /\xFF/.exec("\u00FF");
assert.sameValue(arr[0], "\u00FF");

arr = /\u00FF/.exec("\xFF");
assert.sameValue(arr[0], "\u00FF");

arr = /\u00FF/.exec("\u00FF");
assert.sameValue(arr[0], "\u00FF");

arr = /[\xFF]/.exec("\u00FF");
assert.sameValue(arr[0], "\u00FF");

arr = /[\u00FF]/.exec("\xFF");
assert.sameValue(arr[0], "\u00FF");

var ret = String.fromCharCode(255);
assert.sameValue(ret, "\u00FF");
assert.sameValue(ret, "\xFF");

/* Complicated to implement
arr = /á/i.exec("Á");
assert.sameValue(arr[0], "Á");
*/