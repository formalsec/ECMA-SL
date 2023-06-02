var source1___instr_symb_str_0 = symb_string(source1___instr_symb_str_0);
var source2___instr_symb_str_1 = symb_string(source2___instr_symb_str_1);
Assume(not(source2___instr_symb_str_1 = "valueOf"));
Assume(not(source2___instr_symb_str_1 = "toString"));
Assume(not(source2___instr_symb_str_1 = "hasOwnProperty"));
Assume(not(source2___instr_symb_str_1 = "constructor"));

const f = function (source1, source2) {
	const Func = function () {
	};
	const v1 = Func.prototype;
	v1.x = '2';
	const myFunc = new Func();
	if (source1) {
		const v2 = myFunc.x;
		myFunc[source2] = v2 + source1;
	}
	const v3 = myFunc.x;
	const instr_test_0 = !is_symbolic(v3);
	Assert(instr_test_0);
	const v4 = eval(v3);
	return v4;
};

f(source1___instr_symb_str_0, source2___instr_symb_str_1);
