var p = new Promise(function (resolve, reject) {
	throw Error("whoops!");
});

p.catch(function (v) {
	assert.sameValue(v, "Error: whoops!");

	console.log("I got: " + v);
});

