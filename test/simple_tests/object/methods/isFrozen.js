var obj1 = { bar: 1}

Object.freeze(obj1)

var isFrozen = Object.isFrozen(obj1)

console.log(isFrozen)
