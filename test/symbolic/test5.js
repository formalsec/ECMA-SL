var x = esl_symbolic.number("int", "x");
var y = x * 2;
if (x > 10) {
  esl_symbolic.assert( y < 20 );
} else {
  esl_symbolic.assert( y <= 20 );
}
