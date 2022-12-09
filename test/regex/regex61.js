var ret = "abcA".replace(/a/ig, foo);

console.log(ret);

assert.sameValue(ret, 'yobcyo');

function foo(str, index, input) {
	return "yo";
}

var ret = "abcA".replace(/a/ig, "d");

assert.sameValue(ret, 'dbcd');