var s___instr_symb_str_0 = symb_string(s___instr_symb_str_0);

const read = require('../../aux-files/external-read');
const f = function () {
	const s = read();
	const instr_test_0 = !is_symbolic(s);
	Assert(instr_test_0);
	const v1 = eval(s);
	return v1;
};

read(s___instr_symb_str_0);
