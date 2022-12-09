var aString = new String("test string");

var ret = aString.search(/String/i);

console.log(ret);

assert.sameValue(ret, 5);