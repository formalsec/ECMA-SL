var c___instr_obj_0 = {};
var any_property___instr_symb_0 = symb(any_property___instr_symb_0);
c___instr_obj_0.instr_any_prop_0 = any_property___instr_symb_0;
var p___instr_symb_str_0 = symb_string(p___instr_symb_str_0);
Assume(not(p___instr_symb_str_0 = "valueOf"));
Assume(not(p___instr_symb_str_0 = "toString"));
Assume(not(p___instr_symb_str_0 = "hasOwnProperty"));
Assume(not(p___instr_symb_str_0 = "constructor"));

const f = function (c, p) {
	let x = c[p];
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const v1 = eval(x);
	return v1;
};

f(c___instr_obj_0, p___instr_symb_str_0);
