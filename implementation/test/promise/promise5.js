var f;

var p = new Promise(function (resolve, reject) {
	f = resolve;
});

p.then(function (v) {
	assert.sameValue(v, 5);
	console.log("I got: " + v);
	return v*2;
}).then(function (v) {
	assert.sameValue(v, 10);
	console.log("I got: " + v);
});

f(5);