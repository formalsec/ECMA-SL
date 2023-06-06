var p___instr_symb_str_0 = symb_string(p___instr_symb_str_0);

const f = function (p) {
	const a = [
		0,
		0
	];
	const c = a;
	a[0] = p;
	const v1 = c[0];
	const instr_test_0 = !is_symbolic(v1);
	Assert(instr_test_0);
	const v2 = eval(v1);
	return v2;
};

f(p___instr_symb_str_0);
