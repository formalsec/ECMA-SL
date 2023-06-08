var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const f = function (x) {
	const v1 = `${ x }`;
	const instr_test_0 = !is_symbolic(v1);
	Assert(instr_test_0);
	const v2 = eval(v1);
	return v2;
};

f(x___instr_symb_str_0);
