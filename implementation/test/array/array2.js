var arr = [1 , 2 , 3];
Object.defineProperty (arr, "66", {
	value : 666,
	writable : true,
	enumerable : true,
	configurable : false});
arr.length = 2;

assert.sameValue(arr.length, 67);