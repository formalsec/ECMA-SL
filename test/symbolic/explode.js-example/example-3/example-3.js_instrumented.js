var o___instr_obj_0 = {};
var z___instr_symb_num_0 = esl_symbolic.number("z");
var y___instr_symb_str_0 = esl_symbolic.string("y");
var w___instr_symb_str_1 = esl_symbolic.string("w");
o___instr_obj_0.z = z___instr_symb_num_0;
o___instr_obj_0.y = y___instr_symb_str_0;
o___instr_obj_0.w = w___instr_symb_str_1;

const f = function (o) {
	const v1 = o.z;
	const v2 = v1 > 0;
	if (v2) {
		o.y = '2';
		const v3 = o.y;
		const v4 = o.w;
		const v5 = v3 + v4;
		const v6 = esl_symbolic.evalWrapper(v5);
		return v6;
	}
};

f(o___instr_obj_0);
