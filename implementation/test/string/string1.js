var s = new String("abc");
s[0] = "d";
assert.sameValue(s[0], "a");

s = "def";
s[0] = "a";
assert.sameValue(s[0], "d");