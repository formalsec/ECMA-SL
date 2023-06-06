const yargs = require('yargs');
const v1 = yargs.argv;
const v2 = v1.payload;
const v3 = eval(v2);
v3;