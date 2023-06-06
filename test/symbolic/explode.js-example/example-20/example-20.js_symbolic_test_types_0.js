var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const f = function (x) {
	this.input = x;
	const v4 = function () {
		let self = this;
		const v2 = self.input;
		const instr_test_0 = !is_symbolic(v2);
		Assert(instr_test_0);
		const v3 = eval(v2);
		v3;
	};
	v1.ev = v4;
};

f(x___instr_symb_str_0);
