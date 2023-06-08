var source1___instr_symb_str_0 = esl_symbolic.string("source1");
var source2___instr_symb_str_1 = esl_symbolic.string("source2");
esl_symbolic.assume(!(source2___instr_symb_str_1 == "valueOf"));
esl_symbolic.assume(!(source2___instr_symb_str_1 == "toString"));
esl_symbolic.assume(!(source2___instr_symb_str_1 == "hasOwnProperty"));
esl_symbolic.assume(!(source2___instr_symb_str_1 == "constructor"));

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
	const v4 = esl_symbolic.evalWrapper(v3);
	return v4;
};

f(source1___instr_symb_str_0, source2___instr_symb_str_1);
