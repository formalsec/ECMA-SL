var ret = "abc".split(/a/ig);

var expected = ['', 'bc'];

checkRes();

ret = "abc".split("b");

expected = ['a', 'c'];

checkRes();

function checkRes() {
	for (var index = 0; index < expected.length; index++) {
		if (ret[index] !== expected[index]) {
			$ERROR('#3.' + index + ': ret[index] === ' + expected[index] + '. Actual: ' + ret[index]);
		}
	}
}
