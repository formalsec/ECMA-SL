let esl_symbolic = require("esl_symbolic");

let x = esl_symbolic.string("x");
x.replace(/"/g, '\\"');
