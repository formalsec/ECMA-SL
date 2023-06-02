var x___instr_obj_0 = {};
var y___instr_symb_str_0 = esl_symbolic.string("y");
x___instr_obj_0.y = y___instr_symb_str_0;

const f = function (x) {
	const v1 = x.y;
	const v2 = '(' + v1;
	const v3 = v2 + ')';
	const v4 = esl_symbolic.evalWrapper(v3);
	x.y = v4;
	const v5 = x.y;
	return v5;
};

f(x___instr_obj_0);
