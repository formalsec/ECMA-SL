// retval = 1
var retval_1 = 0;
if (true) {
  retval_1 = 1;
} else {
  retval_ = 2;
}


// retval = 20
var retval_2 = 0;
if (false) {
  retval_2 = 10;
} else {
  retval_2 = 20;
}


// retval = 300
var retval_3 = 300
if (false) {
  retval_3 = 100
}


// retval = 4000
var retval_4 = (true ? 4000 : 5000)


// retval = 50000
var retval_5 = (false ? 40000 : 50000)


// retval = 600000
var cond = 2;
var retval_6 = 0
switch (cond) {
  case 0:
    retval_6 = 0
  case 1:
    retval_6 = 100000;
  case 2:
    retval_6 = 200000;
  case 3:
    retval_6 += 300000;
  default:
    retval_6 += 100000;
}


// retval = 7000000
var cond = 1;
var retval_7 = 0
switch (cond) {
  case 0:
    retval_7 = 0;
    break;
  case 1:
    retval_7 = 7000000;
    break;
  case 2:
    retval_7 = 0;
    break;
  default:
    retval_7 = 0;
}



// retval = 7654321
retval_1 + retval_2 + retval_3 + retval_4 + retval_5 + retval_6 + retval_7;