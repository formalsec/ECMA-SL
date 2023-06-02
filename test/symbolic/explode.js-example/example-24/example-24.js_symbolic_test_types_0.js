var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const v1 = require('child_process');
const execSync = v1.execSync;
const f = function (x) {
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const v2 = execSync(x);
	v2;
};

f(x___instr_symb_str_0);
