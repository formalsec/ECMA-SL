assert.sameValue("\xFF", "\u00FF");

var arr = /\xFF/.exec("\u00FF");
assert.sameValue(arr[0], "\u00FF");