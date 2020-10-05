
// node parse_esl <file_to_parse>
var file_to_parse  = process.argv[2];


fs.readFile(file_to_parse, 'utf8', parseFile);

function parseFile(obj) {
    var prog = Program.fromJSON(obj);
    console.log(prog.toString()); 
    // ... chamar o interpretador 
}