var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const cp = require('child_process');
const f = function (x) {
	var y;
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	y = cp.execSync(x);
};

f(x___instr_symb_str_0);
