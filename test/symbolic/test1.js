// test1.js: Test symbolic value creation

var x = esl_symbolic.number("int", "x");
esl_symbolic.assume( x > 0 );
var y = x + 1;
esl_symbolic.assert( y > 0 );
