// retval = 1
var object_1 = { field: 1 };
var retval_1 = object_1.field;



// retval = 20
var object_2 = {};
object_2.field = 20;
var retval_2 = object_2.field;



// retval = 300
var object_3 = { field: 300 };
var retval_3 = object_3["field"];



// retval = 4000
function aux_4(n) {
  return n * 1000;
}
var object_4 = { fun: aux_4 };
var retval_4 = object_4.fun(4);



// retval = 50000
var object_5_proto = { field: 50000 };
var object_5 = {};
object_5.__proto__ = object_5_proto;
var retval_5 = object_5.field;


// retval = 54321
retval_1 + retval_2 + retval_3 + retval_4 + retval_5;