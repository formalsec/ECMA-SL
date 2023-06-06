var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const f = function (x) {
	this.input = x;
	const v3 = function () {
		const v1 = this.input;
		const instr_test_0 = !is_symbolic(v1);
		Assert(instr_test_0);
		const v2 = eval(v1);
		return v2;
	};
	return v3;
};

f(x___instr_symb_str_0);
