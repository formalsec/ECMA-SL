// var esl_symbolic = require('esl_symbolic');
"use strict";

Object.freeze(Object.prototype);

// const hOp = Object.hasOwnProperty;
// Object.defineProperty(Object.prototype, "hasOwnProperty",
// 	{ get : function() { return hOp; }
// 	, set : function() { throw "Prototype Pollution Exploit"; }
// 	});

// var o = {};
// // var p0 = esl_symbolic.string("p0");
// // var p1 = esl_symbolic.string("p1");

var o = {};
o.constructor.prototype.banana= 'polluted';

console.log("TO STRING:");
console.log(({}).banana);
// console.log(({}).constructor.prototype.toString);
