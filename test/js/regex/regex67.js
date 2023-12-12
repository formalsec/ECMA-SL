var ret = "abcA".replace(/a/ig, "d");

assert.sameValue(ret, 'dbcd');