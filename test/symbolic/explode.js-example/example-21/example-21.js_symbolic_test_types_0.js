var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const child_process = require('child_process');
const f = function (x) {
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const v1 = child_process.execSync(x);
	v1;
};

f(x___instr_symb_str_0);
