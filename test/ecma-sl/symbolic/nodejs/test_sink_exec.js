let esl_symbolic = require("esl_symbolic");
let child_process = require("child_process");
let source = esl_symbolic.string("source");
child_process.exec(source);
