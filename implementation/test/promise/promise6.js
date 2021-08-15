/**
 * The Promise.race() method returns a promise that fulfills
 * or rejects as soon as one of the promises in an iterable
 * fulfills or rejects, with the value or reason from that promise. 
 */

var f;

var p1 = new Promise(function (resolve, reject) {
	f = resolve;
});

var p2 = new Promise(function (resolve, reject) {
	resolve(10);
});

Promise.race([p1, p2]).then(function(v) {
	assert.sameValue(v, 10);
	console.log("I got: " + v);
	// Both resolve, but p2 is faster
});

f(5);