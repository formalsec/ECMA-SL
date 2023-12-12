function foo(x) {
	var y = 10;
	function bar() {
		return x + y;
	}

	return bar;
}

var f = foo(10);
f();
