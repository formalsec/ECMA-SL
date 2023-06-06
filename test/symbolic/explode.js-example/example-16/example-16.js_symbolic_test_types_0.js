var x___instr_symb_str_0 = symb_string(x___instr_symb_str_0);

const Cla = function Cla() {
};
const ev = function ev(x) {
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const v1 = eval(x);
	return v1;
};
Cla.ev = ev;

ev(x___instr_symb_str_0);
