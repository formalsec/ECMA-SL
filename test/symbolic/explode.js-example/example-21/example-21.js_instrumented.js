var x___instr_symb_str_0 = esl_symbolic.string("x");

//ignore: const child_process = require('child_process');
const f = function (x) {
	const instr_test_0 = !esl_symbolic.is_symbolic(x);
	esl_symbolic.assert(instr_test_0);
	const v1 = child_process.execSync(x);
	v1;
};

f(x___instr_symb_str_0);
