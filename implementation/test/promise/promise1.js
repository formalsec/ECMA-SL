var p = new Promise(function (resolve, reject) {
	resolve(5);
});

p.then(function (v) {
	console.log("I got: " + v);
});