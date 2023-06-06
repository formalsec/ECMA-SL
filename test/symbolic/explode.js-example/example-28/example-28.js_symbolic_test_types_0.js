var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);
var y___instr_obj_0 = {};

const exec = require('cross-spawn');
const f = function (x, y) {
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const instr_test_1 = !is_symbolic(y);
	Assert(instr_test_1);
	const v1 = exec(x, y);
	v1;
};

f(x___instr_symb_str_0, y___instr_obj_0);
