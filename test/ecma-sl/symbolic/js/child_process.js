var esl_symbolic = require("esl_symbolic");
var child_process = require("child_process");

let source = esl_symbolic.string("source");
child_process.exec(source);
