// test3.js: while-loop

let x = esl_symbolic.number("int", "x");
esl_symbolic.assume( x > 1 && x < 10 );
let i = 0;
while (i < x) {
  i = i + 1;
}
