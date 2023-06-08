var obj___instr_obj_0 = {};
var prop___instr_symb_str_0 = esl_symbolic.string("prop___instr_symb_str_0");
obj___instr_obj_0.prop = prop___instr_symb_str_0;

const f = function (obj) {
	const v1 = !obj;
	if (v1) {
		obj = {};
		obj.prop = '2+2';
	}
	const v2 = obj.prop;
	const v3 = esl_symbolic.evalWrapper(v2);
	return v3;
};

f(obj___instr_obj_0);
