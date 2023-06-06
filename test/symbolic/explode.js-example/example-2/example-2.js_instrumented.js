var y___instr_symb_str_0 = esl_symbolic.string("y");

const f = function (y) {
	let x = {};
	x.f = y;
	let o = x;
	const v1 = o.f;
	const v2 = esl_symbolic.evalWrapper(v1);
	return v2;
};

f(y___instr_symb_str_0);
