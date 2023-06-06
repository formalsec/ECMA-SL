var x___instr_symb_str_0 = esl_symbolic.string("x");

// ignore: const cp = require('child_process');
const f = function (x) {
	var y;
	const instr_test_0 = !esl_symbolic.is_symbolic(x);
	esl_symbolic.assert(instr_test_0);
	y = cp.execSync(x);
};

f(x___instr_symb_str_0);
