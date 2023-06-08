var obj___instr_obj_0 = {};
var inputs___instr_obj_1 = {};
var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);
var y___instr_symb_0 = symb(y___instr_symb_0);
inputs___instr_obj_1.x = x___instr_symb_str_0;
inputs___instr_obj_1.y = y___instr_symb_0;
var conds___instr_obj_2 = {};
var cond1___instr_symb_num_0 = symb_number(cond1___instr_symb_num_0);
conds___instr_obj_2.cond1 = cond1___instr_symb_num_0;
var z___instr_symb_1 = symb(z___instr_symb_1);
obj___instr_obj_0.inputs = inputs___instr_obj_1;
obj___instr_obj_0.conds = conds___instr_obj_2;
obj___instr_obj_0.z = z___instr_symb_1;
var malicious___instr_symb_str_1 = symb_string(malicious___instr_symb_str_1);

const f = function (obj, malicious) {
	const v1 = obj.inputs;
	v1.x = malicious;
	const v2 = obj.conds;
	if (v2) {
		const v3 = obj.conds;
		const v4 = v3.cond1;
		const v5 = v4 * 10;
		const v6 = v5 >= 100;
		if (v6) {
			const v7 = obj.inputs;
			const v8 = v7.x;
			const instr_test_0 = !is_symbolic(v8);
			Assert(instr_test_0);
			const v9 = eval(v8);
			return v9;
		} else {
			const v10 = obj.inputs;
			const v11 = v10.y;
			return v11;
		}
	} else {
		const v12 = obj.z;
		return v12;
	}
};

f(obj___instr_obj_0, malicious___instr_symb_str_1);
