var re = new RegExp("/");

var expected = /\//;

assert.sameValue(re.source, expected.source);