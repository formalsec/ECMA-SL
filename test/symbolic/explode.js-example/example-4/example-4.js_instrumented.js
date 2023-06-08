var req___instr_obj_0 = {};
var body___instr_obj_1 = {};
var a___instr_symb_num_0 = esl_symbolic.number("a");
var param___instr_symb_str_0 = esl_symbolic.string("param");
body___instr_obj_1.a = a___instr_symb_num_0;
body___instr_obj_1.param = param___instr_symb_str_0;
req___instr_obj_0.body = body___instr_obj_1;

const f = function (req) {
	const x = req.body;
	x.a = 2;
	const v1 = x.param;
	const v2 = esl_symbolic.evalWrapper(v1);
	return v2;
};

f(req___instr_obj_0);
