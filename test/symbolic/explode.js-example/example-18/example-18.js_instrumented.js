var x___instr_symb_str_0 = esl_symbolic.string("x");

const f = function (x) {
	let evalArgs;
	const v1 = x.split(' ');
	if (true) {
		evalArgs = x;
	} else {
		evalArgs = v1;
	}
	const v2 = esl_symbolic.evalWrapper(evalArgs);
	return v2;
};

f(x___instr_symb_str_0);
