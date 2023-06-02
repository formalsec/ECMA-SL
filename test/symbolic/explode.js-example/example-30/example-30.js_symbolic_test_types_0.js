var p___instr_symb_str_0 = symb_string(p___instr_symb_str_0);
Assume(not(p___instr_symb_str_0 = "valueOf"));
Assume(not(p___instr_symb_str_0 = "toString"));
Assume(not(p___instr_symb_str_0 = "hasOwnProperty"));
Assume(not(p___instr_symb_str_0 = "constructor"));
var z___instr_symb_str_1 = symb_string(z___instr_symb_str_1);

const f = function (p, z) {
	const c = {};
	c[p] = z;
	let x = c['w'];
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const v1 = eval(x);
	return v1;
};

f(p___instr_symb_str_0, z___instr_symb_str_1);
