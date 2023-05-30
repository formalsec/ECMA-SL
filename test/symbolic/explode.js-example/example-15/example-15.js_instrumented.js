
const process = require('process');
const v1 = process.argv;
const x = v1[2];
{
	const v2 = esl_symbolic.evalWrapper(x);
}
v2;

v1();
