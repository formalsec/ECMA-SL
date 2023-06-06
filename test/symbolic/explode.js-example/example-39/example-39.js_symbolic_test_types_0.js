var arg0___instr_symb_str_0 = symb_string(arg0___instr_symb_str_0);
var arg1___instr_obj_0 = {};
var prop___instr_symb_0 = symb(prop___instr_symb_0);
arg1___instr_obj_0.prop = prop___instr_symb_0;
var len___instr_symb_num_0 = symb_number(len___instr_symb_num_0);
var v4___instr_obj_1 = {};
var prop___instr_symb_1 = symb(prop___instr_symb_1);
v4___instr_obj_1.prop = prop___instr_symb_1;
var v7___instr_obj_2 = {};
var prop___instr_symb_str_1 = symb_string(prop___instr_symb_str_1);
v7___instr_obj_2.prop = prop___instr_symb_str_1;

const f = function () {
	arg0 = arguments[0];
	arg1 = arguments[1];
	len = arguments.length;
	var x = arg1;
	let i = 0;
	let v1 = i < len;
	while (v1) {
		const v3 = x.prop;
		const v4 = arguments[i];
		const v5 = v4.prop;
		const v6 = v3 == v5;
		if (v6) {
			const v7 = arguments[i];
			const v8 = v7.prop;
			const instr_test_0 = !is_symbolic(v8);
			Assert(instr_test_0);
			const v9 = eval(v8);
			return v9;
		}
		const v2 = i++;
		v1 = i < len;
	}
};

f(arg0___instr_symb_str_0, arg1___instr_obj_0, len___instr_symb_num_0, v4___instr_obj_1, v7___instr_obj_2);
