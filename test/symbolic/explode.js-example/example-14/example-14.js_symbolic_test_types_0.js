var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const f = function (x) {
	try {
		const instr_test_0 = !is_symbolic(x);
		Assert(instr_test_0);
		const v1 = eval(x);
		return v1;
	} catch (e) {
		const v2 = console.log(e);
		v2;
	}
};

f(x___instr_symb_str_0);
