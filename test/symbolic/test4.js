// test4.js: with new context
function myabs(x) {
  if (x >= 0) {
    return x;
  }
  return -x;
}

var x = esl_symbolic.number("x");
esl_symbolic.assert( myabs(x) > 0 );
