// Summary: child_process.execSync
var child_process = {};
child_process.execSync = function (x) { return esl_symbolic.execWrapper(x) };

// Test21
var x___instr_symb_str_0 = esl_symbolic.string("x");
const f = function (x) {
	const v1 = child_process.execSync(x);
	v1;
};

f(x___instr_symb_str_0);
