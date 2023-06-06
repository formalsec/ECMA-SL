var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const child_process = require('child_process');
const f = function (x) {
	cmd = [
		'cat',
		'-n'
	];
	const v1 = cmd.push(x);
	v1;
	const v2 = cmd.join(' ');
	const instr_test_0 = !is_symbolic(v2);
	Assert(instr_test_0);
	const v3 = child_process.execSync(v2);
	v3;
};

f(x___instr_symb_str_0);
