/**
 * * javascript/simple/map.js
 * 
 * Simple implementation of a concrete map with functions to store and retrieve bindings. 
 * @return true
*/

function Map() {
	this._contents = {};
}

function isValidKey(key) {
	return typeof key === 'string' && key !== '';
}


Map.prototype.get = function get(key) {
	if (isValidKey(key)) {
		if (this._contents.hasOwnProperty(key)) {
			return this._contents[key];
		} else {
			return null;
		}
	} else {
		throw new Error("invalid key")
	}
}


Map.prototype.put = function put(key, value) {
	if (isValidKey(key)) {
		this._contents[key] = value;
	} else {
		throw new Error("invalid key")
	}
}

let map = new Map();
map.put("foo", 10);
map.put("bar", 20);

let total = map.get("foo") + map.get("bar");

AssertEquals(total, 30);
