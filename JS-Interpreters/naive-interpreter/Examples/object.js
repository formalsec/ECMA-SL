function cmp(other) {
	return this.foo == other.foo;
}

function Obj(foo) {
	this.foo = foo;
	this.bar = "abc";
  this.cmp = cmp;
}

x = new Obj(10);
y = new Obj(20);
x.cmp(y);