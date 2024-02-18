/**
 * * javascript/simple/harness.js
 * 
 * Harness for the simple javascript tests.
*/

function AssertTrue(val) {
	return val === true;
}

function AssertEquals(val, exp) {
	return val === exp;
}

function AssertArray(arr, exp) {
	return arr.length === exp.length &&
		arr.every((element, index) => element === exp[index]);
}

function AssertObject(obj, exp) {
	return Object.keys(obj).length === Object.keys(exp).length &&
		Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])
}
