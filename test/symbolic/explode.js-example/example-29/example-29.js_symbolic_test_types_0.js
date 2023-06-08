var o___instr_obj_0 = {};
var z___instr_obj_1 = {};
var cond1___instr_symb_bool_0 = symb_bool(cond1___instr_symb_bool_0);
z___instr_obj_1.cond1 = cond1___instr_symb_bool_0;
var y___instr_symb_str_0 = symb_string(y___instr_symb_str_0);
var w___instr_symb_str_1 = symb_string(w___instr_symb_str_1);
var x___instr_symb_str_2 = symb_string(x___instr_symb_str_2);
o___instr_obj_0.z = z___instr_obj_1;
o___instr_obj_0.y = y___instr_symb_str_0;
o___instr_obj_0.w = w___instr_symb_str_1;
o___instr_obj_0.x = x___instr_symb_str_2;

const f = function (o) {
	const v1 = o.z;
	const v2 = v1 > 0;
	if (v2) {
		o.y = '2';
		const v3 = o.y;
		const v4 = o.w;
		const v5 = v3 + v4;
		const instr_test_0 = !is_symbolic(v5);
		Assert(instr_test_0);
		const v6 = eval(v5);
		return v6;
	} else {
		const v7 = o.z;
		const v8 = v7.cond1;
		if (v8) {
			const v9 = o.x;
			const v10 = eval(v9);
			return v10;
		}
	}
};

f(o___instr_obj_0);
