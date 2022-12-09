var s = new String("abc");
var count = 0;
for (c in s) { count++; }
assert.sameValue(count, 3);