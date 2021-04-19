var x;

var re = new RegExp(x, "g");

var expected = /(?:)/g;

assert.sameValue(re.source, expected.source);

var str = String("asdf");

assert.sameValue(str, "asdf");

var replaced = str.replace(re, "1");

console.log(replaced);

assert.sameValue(replaced, "1a1s1d1f1");