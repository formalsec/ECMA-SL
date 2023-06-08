var obj1___instr_obj_0 = {};
var prop___instr_symb_0 = esl_symbolic.string("prop___instr_symb_0");
obj1___instr_obj_0.prop = prop___instr_symb_0;
var obj2___instr_obj_1 = {};
var prop___instr_symb_1 = esl_symbolic.string("prop___instr_symb_1");
obj2___instr_obj_1.prop = prop___instr_symb_1;

const f = function (obj1, obj2) {
	obj1 = obj1 || obj2;
	const v1 = obj1.prop;
	const v2 = esl_symbolic.evalWrapper(v1);
	return v2;
};

f(obj1___instr_obj_0, obj2___instr_obj_1);
