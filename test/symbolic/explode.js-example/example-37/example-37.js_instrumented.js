
const v1 = process.argv;
const v2 = v1[2];
{
	const instr_test_0 = !esl_symbolic.is_symbolic(v2);
	esl_symbolic.assert(instr_test_0);
	const v3 = esl_symbolic.evalWrapper(v2);
}
v3;

v1();
