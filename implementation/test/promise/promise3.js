var p = new Promise(function (resolve, reject) {
	throw Error("whoops!");
});

p.catch(function (v) {
	//assert.sameValue(v, 4);
	console.log("I got: " + v);
});