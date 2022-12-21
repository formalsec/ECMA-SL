var p = new Promise(function (resolve, reject) {
	reject(4);
});

p.then(undefined, function (v) {
	assert.sameValue(v, 4);
	console.log("I got: " + v);
});