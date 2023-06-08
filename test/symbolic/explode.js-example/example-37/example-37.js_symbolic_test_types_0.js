
const v1 = process.argv;
const v2 = v1[2];
{
	const instr_test_0 = !is_symbolic(v2);
	Assert(instr_test_0);
	const v3 = eval(v2);
}
v3;

v1();
