function f () {
  this.bar = 2  
}

f.prototype = { foo: 1 }

var o = new f()

console.log(o.foo + o.bar)

