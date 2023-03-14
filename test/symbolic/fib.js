function fib_ite(n) {
  let a = 0, b = 1, c = 0;
  for (let i = 2; i <= n; i++) {
    c = a + b;
    a = b;
    b = c;
  }
  return c;
}

var x = esl_symbolic.number("int", "x");
var result = fib_ite(x);
if ( x >= 8 && result < 34 ) {
  esl_symbolic.assert( false );
}
