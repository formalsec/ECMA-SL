// test3.js: while-loop

let x = esl_symbolic.number("x");
esl_symbolic.assume( x > 1 && x < 10 );
let i = 0;
while (i < x) {
  i = i + 1;
}
esl_symbolic.assert( i != 10 );
