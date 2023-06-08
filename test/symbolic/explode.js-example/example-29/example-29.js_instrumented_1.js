var o___instr_obj_2 = {};
var z___instr_obj_3 = {};
var cond1___instr_symb_bool_1 = esl_symbolic.bool("cond1");
z___instr_obj_3.cond1 = cond1___instr_symb_bool_1;
var y___instr_symb_str_3 = esl_symbolic.string("y");
var w___instr_symb_str_4 = esl_symbolic.string("w");
var x___instr_symb_str_5 = esl_symbolic.string("x");
o___instr_obj_2.z = z___instr_obj_3;
o___instr_obj_2.y = y___instr_symb_str_3;
o___instr_obj_2.w = w___instr_symb_str_4;
o___instr_obj_2.x = x___instr_symb_str_5;

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
	} else {
		const v7 = o.z;
		const v8 = v7.cond1;
		if (v8) {
			const v9 = o.x;
			const instr_test_1 = !esl_symbolic.is_symbolic(v9);
			esl_sybmolic.assert(instr_test_1);
			const v10 = esl_symbolic.evalWrapper(v9);
			return v10;
		}
	}
};

f(o___instr_obj_2);
