var x___instr_obj_0 = {};
var cmd___instr_symb_str_0 = esl_symbolic.string("cmd");
x___instr_obj_0.cmd = cmd___instr_symb_str_0;

// ignore: var child = require('child_process');
const f1 = function (x) {
	const cmd = x.cmd;
	const f2 = function (y) {
		const instr_test_0 = !esl_symbolic.is_symbolic(y);
		esl_symbolic.assert(instr_test_0);
		const v1 = child.execSync(y);
		v1;
	};
	const v2 = f2(cmd);
	v2;
};

f1(x___instr_obj_0);
