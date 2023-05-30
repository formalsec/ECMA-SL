var x___instr_symb_str_0 = esl_symbolic.string("x");

const f = function (x) {
	this.input = x;
	const v3 = function () {
		const v1 = this.input;
		const v2 = esl_symbolic.evalWrapper(v1);
		return v2;
	};
	return v3;
};

f(x___instr_symb_str_0)();
