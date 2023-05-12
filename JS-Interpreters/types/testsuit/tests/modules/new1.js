function Person(name, age) {
  this.name = name; 
  this.age = age
}



Person.prototype.sayHi = function () {
  return this.name + " " + this.age
}

var p = new Person("maria", 23); 

var s = p.sayHi(); 

// maria 23
s
