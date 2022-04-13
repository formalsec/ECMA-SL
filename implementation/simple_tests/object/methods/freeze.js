var obj1 = { bar: 1}

Object.freeze(obj1)

obj1.bar = 2
console.log(obj1.bar)
