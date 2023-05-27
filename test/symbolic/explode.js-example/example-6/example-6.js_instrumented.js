var s___instr_symb_str_0 = esl_symbolic.string("s");

const read = function () {
	return '2+2';
};
const f = function () {
	const s = read();
	const v1 = esl_symbolic.evalWrapper(s);
	return v1;
};

read(s___instr_symb_str_0);
