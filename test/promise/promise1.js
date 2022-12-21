var p = new Promise(function (resolve, reject) {
	resolve(5);
});

p.then(function (v) {
	assert.sameValue(v, 5);
	console.log("I got: " + v);
});