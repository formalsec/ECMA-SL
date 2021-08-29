/**
 * The Promise.all() method takes an iterable of promises as an input,
 * and returns a single Promise that resolves to an array of the results
 * of the input promises. This returned promise will resolve when all of
 * the input's promises have resolved, or if the input iterable contains
 * no promises. It rejects immediately upon any of the input promises
 * rejecting or non-promises throwing an error, and will reject with
 * this first rejection message / error. 
 */

var promise1 = Promise.resolve(3);
var promise2 = 42;
var promise3 = new Promise(function(resolve, reject) {
	resolve('foo');
});

Promise.all([promise1, promise2, promise3]).then(function(values) {
	assert.sameValue(values, [3, 42, "foo"]);
	console.log(values);
});
// expected output: Array [3, 42, "foo"]