var obj___instr_obj_2 = {};
var obj____inputs___instr_obj_0 = {};
var obj____inputs____x___instr_symb_str_0 = esl_symbolic.string("obj____inputs____x___instr_symb_str_0");
obj____inputs___instr_obj_0.x = obj____inputs____x___instr_symb_str_0;
var obj____conds___instr_obj_1 = {};
var obj____conds____cond1___instr_symb_num_0 = esl_symbolic.number("obj____conds____cond1___instr_symb_num_0");
obj____conds___instr_obj_1.cond1 = obj____conds____cond1___instr_symb_num_0;
obj___instr_obj_2.inputs = obj____inputs___instr_obj_0;
obj___instr_obj_2.conds = obj____conds___instr_obj_1;
var malicious___instr_symb_0 = esl_symbolic.string("malicious___instr_symb_0");

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
			const v9 = esl_symbolic.evalWrapper(v8);
			return v9;
		}
	}
};

f(obj___instr_obj_2, malicious___instr_symb_0);
