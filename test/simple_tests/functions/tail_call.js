function f() {
	return 1
}

function g() {
	return f()
}

var x = g();

console.log(x);