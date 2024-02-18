/**
 * * javascript/simple/harness.js
 * 
 * Harness for the simple javascript tests.
*/

function AssertEquals(x, y) {
	return x === y;
}

function CompareArrays(x, y) {
	return x.length === y.length &&
		x.every((element, index) => element === y[index]);
}