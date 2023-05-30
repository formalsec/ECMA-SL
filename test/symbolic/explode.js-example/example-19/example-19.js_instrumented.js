var x___instr_symb_str_0 = esl_symbolic.string("x");

const f = function (x) {
	const instr_test_0 = !esl_symbolic.is_symbolic(x);
	esl_symbolic.assert(instr_test_0);
	var func = new Function(x);
	const v1 = func();
	return v1;
};

f(x___instr_symb_str_0);
