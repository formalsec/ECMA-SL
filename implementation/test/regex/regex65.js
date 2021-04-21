var r = new RegExp(/\n+/gim);

var expected = /\n+/gim;

console.log(r.source);
console.log(expected.source);

assert.sameValue(r.source, expected.source);