var c___instr_obj_0 = {};
var any_property___instr_symb_0 = esl_symbolic.number("any");
c___instr_obj_0.instr_any_prop_0 = any_property___instr_symb_0;
var p___instr_symb_str_0 = esl_symbolic.string("p");
esl_symbolic.assume(!(p___instr_symb_str_0 == "valueOf"));
esl_symbolic.assume(!(p___instr_symb_str_0 == "toString"));
esl_symbolic.assume(!(p___instr_symb_str_0 == "hasOwnProperty"));
esl_symbolic.assume(!(p___instr_symb_str_0 == "constructor"));

const f = function (c, p) {
	let x = c[p];
	const v1 = esl_symbolic.evalWrapper(x);
	return v1;
};

f(c___instr_obj_0, p___instr_symb_str_0);
