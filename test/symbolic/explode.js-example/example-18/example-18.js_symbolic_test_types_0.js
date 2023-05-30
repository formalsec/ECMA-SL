var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const f = function (x) {
	let evalArgs;
	const v1 = x.split(' ');
	if (true) {
		evalArgs = x;
	} else {
		evalArgs = v1;
	}
	const instr_test_0 = !is_symbolic(evalArgs);
	Assert(instr_test_0);
	const v2 = eval(evalArgs);
	return v2;
};

f(x___instr_symb_str_0);
