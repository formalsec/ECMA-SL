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



var ObjCons = function (foo) { 
	this.foo = foo;
	this.bar = "def"
}

ObjCons.prototype.cmp = function (other) { this.foo == other.foo }
z = new ObjCons(10);
w = new ObjCons(20);
z.cmp(w);