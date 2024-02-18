/**
 * * javascript/simple/function.js
 * 
 * Function that returns a context-dependent inner function.
 * @return true
*/

function foo(x) {
	let value = x;
	return () => {
		return value + 10;
	};
}

let func = foo(10);
let ret = func();

AssertEquals(ret, 20);
