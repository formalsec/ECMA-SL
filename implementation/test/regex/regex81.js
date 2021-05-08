var executed = /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac");
var expected = ["zaacbbbcac", "z", "ac", "a", undefined, "c"];

console.log(executed);

assert.sameValue(executed, expected);

// "JS Log: zaacbbbcac,z,ac,a,bbb,c"