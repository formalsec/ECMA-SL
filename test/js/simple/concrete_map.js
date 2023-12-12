/*get(m.put(k, v)) = v */


function Map () {
  this._contents = {};
}

function isValidKey (key) {
  return (typeof (key) === 'string') && (key !== '');
}


Map.prototype.get = function get (key) {
  var result;

  if (isValidKey(key)) {
    if (this._contents.hasOwnProperty(key)) {
      result = this._contents[key]
    } else {
      result = null
    }
    return result
  } else {
    throw new Error("Invalid Key")
  }
}


Map.prototype.put = function put (key, value) {
  if (isValidKey(key)) {
    this._contents[key] = value;
  } else {
    throw new Error("Invalid Key")
  }
}

var m = new Map();
m.put("a", "banana");
m.put("b", "passionfruit");
var ret1 = m.get("a");

AssertEquals(ret1, "banana")
