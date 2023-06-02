var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

var child = require('child_process');
const f = function (x) {
	const arr = x.join(' ');
	const instr_test_0 = !is_symbolic(arr);
	Assert(instr_test_0);
	const v1 = child.execSync(arr);
	v1;
};

f(x___instr_symb_str_0);
