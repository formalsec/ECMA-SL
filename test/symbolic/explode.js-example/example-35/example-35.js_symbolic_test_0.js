var arr___instr_symb_0 = symb(arr___instr_symb_0);

var child = require('child_process');
const f1 = function (arr) {
	const v2 = element => {
		const instr_test_0 = !is_symbolic(element);
		Assert(instr_test_0);
		const v1 = child.execSync(element);
		v1;
	};
	const v3 = arr.forEach(v2);
	v3;
};

f1(arr___instr_symb_0);
