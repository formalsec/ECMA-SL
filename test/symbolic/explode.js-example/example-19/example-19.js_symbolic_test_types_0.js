var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const f = function (x) {
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	var func = new Function(x);
	const v1 = func();
	return v1;
};

f(x___instr_symb_str_0);
