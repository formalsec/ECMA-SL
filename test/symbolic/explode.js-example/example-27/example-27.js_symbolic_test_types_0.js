var x___instr_obj_0 = {};
var y___instr_symb_str_0 = symb_string(y___instr_symb_str_0);
x___instr_obj_0.y = y___instr_symb_str_0;

const f = function (x) {
	const v1 = x.y;
	const v2 = '(' + v1;
	const v3 = v2 + ')';
	const instr_test_0 = !is_symbolic(v3);
	Assert(instr_test_0);
	const v4 = eval(v3);
	x.y = v4;
	const v5 = x.y;
	return v5;
};

f(x___instr_obj_0);
