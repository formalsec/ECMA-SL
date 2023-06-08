var x___instr_symb_str_0 = esl_symbolic.string();
var y___instr_obj_0 = {};

// ignore: const exec = require('cross-spawn');
const f = function (x, y) {
	const instr_test_0 = !esl_symbolic.is_symbolic(x);
	esl_symbolic.assert(instr_test_0);
	const instr_test_1 = !esl_symbolic.is_symbolic(y);
	esl_symbolic.assert(instr_test_1);
	const v1 = exec(x, y);
	v1;
};

f(x___instr_symb_str_0, y___instr_obj_0);
