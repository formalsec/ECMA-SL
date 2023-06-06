var p___instr_symb_str_0 = esl_symbolic.string("p");
esl_symbolic.assume(!(p___instr_symb_str_0 == "valueOf"));
esl_symbolic.assume(!(p___instr_symb_str_0 == "toString"));
esl_symbolic.assume(!(p___instr_symb_str_0 == "hasOwnProperty"));
esl_symbolic.assume(!(p___instr_symb_str_0 == "constructor"));
var z___instr_symb_str_1 = esl_symbolic.string(z___instr_symb_str_1);

const f = function (p, z) {
	const c = {};
	c[p] = z;
	let x = c['w'];
	const instr_test_0 = !esl_symbolic.is_symbolic(x);
	esl_symbolic.assert(instr_test_0);
	const v1 = esl_symbolic.evalWrapper(x);
	return v1;
};

f(p___instr_symb_str_0, z___instr_symb_str_1);
