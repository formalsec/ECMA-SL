
const process = require('process');
const v1 = process.argv;
const x = v1[2];
{
	const instr_test_0 = !is_symbolic(x);
	Assert(instr_test_0);
	const v2 = eval(x);
}
v2;

v1();
