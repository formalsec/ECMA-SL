// testing taint from parameter to eval function call
// paremeter passed as argument to process.argv, process isn't required

eval(process.argv[2])
