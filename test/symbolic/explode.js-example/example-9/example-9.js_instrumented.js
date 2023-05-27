var p___instr_symb_0 = esl_symbolic.string("p");

const f = function (p) {
	const a = {};
	a.b = p;
	const c = a.b;
	const v1 = esl_symbolic.evalWrapper(c);
	return v1;
};

f(p___instr_symb_0);
