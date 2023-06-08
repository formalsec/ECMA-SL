var x___instr_symb_str_0 = esl_symbolic.string("x");

const v1 = require('child_process');
const execSync = v1.execSync;
const f = function (x) {
	const instr_test_0 = !esl_symbolic.is_symbolic(x);
	esl_symbolic.assert(instr_test_0);
	const v2 = execSync(x);
	v2;
};

f(x___instr_symb_str_0);
