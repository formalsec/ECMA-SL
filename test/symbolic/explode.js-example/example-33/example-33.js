// testing taint from parameter to eval function call
// paremeter passed as argument to yargs.argv

const yargs = require("yargs")
eval(yargs.argv.payload)
