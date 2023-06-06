var obj___instr_obj_0 = {};
var prop___instr_symb_str_0 = symb_string(prop___instr_symb_str_0);
obj___instr_obj_0.prop = prop___instr_symb_str_0;

const f = function (obj) {
	const v1 = !obj;
	if (v1) {
		obj = {};
		obj.prop = '2+2';
	}
	const v2 = obj.prop;
	const instr_test_0 = !is_symbolic(v2);
	Assert(instr_test_0);
	const v3 = eval(v2);
	return v3;
};

f(obj___instr_obj_0);
