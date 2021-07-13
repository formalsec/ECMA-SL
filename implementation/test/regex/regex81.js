var __executed = /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac");
var __expected = ["zaacbbbcac", "z", "ac", "a", undefined, "c"];

for(var index=0; index<__expected.length; index++) {
	if (__executed[index] !== __expected[index]) {
		$ERROR('#4: __executed = /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac"); __executed[' + index + '] === ' + __expected[index] + '. Actual: ' + __executed[index]);
	}
}