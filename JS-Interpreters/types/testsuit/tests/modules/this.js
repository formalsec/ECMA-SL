// retval = 1
this.retval_1_this = 1;
var retval_1 = this.retval_1_this;



// retval = 20
function retval_2_fun_this() {
  this.retval_2_this = 20;
  return this.retval_2_this;
}
var retval_2 = retval_2_fun_this();



// retval = 300
function retval_3_fun_this() {
  return this.retval_3_this;
}
object_3 = { retval_3_this: 300, fun: retval_3_fun_this }
var retval_3 = object_3.fun();



// retval = 4000
function retval_4_fun_this() {
  this.retval_4_this = 4000;
  return retval_4_this;
}
var retval_4 = retval_4_fun_this();



// retval = 50000
function retval_5_fun_this() {
  retval_5_this = 50000;
  return this.retval_5_this;
}
var retval_5 = retval_5_fun_this();



// retval = 600000
function retval_6_fun_this() {
  this.retval_6_this = 600000;
  function retval_6_fun_this_aux() {
    return retval_6_this;
  }
  return retval_6_fun_this_aux;
}
var retval_6 = retval_6_fun_this()();



retval_1 + retval_2 + retval_3 + retval_4 + retval_5 + retval_6;