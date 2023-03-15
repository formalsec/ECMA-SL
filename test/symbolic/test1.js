// test1.js: Test symbolic value creation

var x = esl_symbolic.number("int", "x");
esl_symbolic.assume( x == 10 );
var y = x * 2;
esl_symbolic.assert( y == 20 );
