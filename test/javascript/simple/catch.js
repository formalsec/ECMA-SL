/**
 * * javascript/simple/catch.js
 * 
 * Function that returns a context-dependent with an exception.
 * @return true
*/

function foo(x) {
	let value = x;
	return () => {
		throw value + 10;
	};
}

let func = foo(10);
let ret = -1;

try {
	ret = func();
} catch (e) {
	ret = e + 10;
}

AssertEquals(ret, 30);
