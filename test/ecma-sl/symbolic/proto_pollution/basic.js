// var esl_symbolic = require('esl_symbolic');

Object.sealProperties(Object.prototype);

// const hOp = Object.hasOwnProperty;
// Object.defineProperty(Object.prototype, "hasOwnProperty",
// 	{ get : function() { return hOp; }
// 	, set : function() { throw "Prototype Pollution Exploit"; }
// 	});

var o = {};
// var p0 = esl_symbolic.string("p0");
// var p1 = esl_symbolic.string("p1");

o.constructor.prototype.toString = 'polluted';
