var x___instr_obj_0 = {};
var so___instr_obj_1 = {};
var cmd___instr_symb_str_0 = esl_symbolic.string();
so___instr_obj_1.cmd = cmd___instr_symb_str_0;
x___instr_obj_0.so = so___instr_obj_1;

// ignore: var child = require('child_process');
const f1 = function (x) {
	const so = x.so;
	const f2 = function (y) {
		const cmd = y.cmd;
		const instr_test_0 = !esl_symbolic.is_symbolic(cmd);
		esl_symbolic.assert(instr_test_0);
		const v1 = child.execSync(cmd);
		v1;
	};
	const v2 = f2(so);
	v2;
};

f1(x___instr_obj_0);
