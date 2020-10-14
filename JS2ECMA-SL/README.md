#ECMA-SL BASE TOOL

##OCAML (ECMA-SL) - implementation

###Install

###Compile Interpreter 

After the corret instalation of OCAML and the needed Lybraries, compile the program:

```
make
```

###Run Interpreter

This program expects as input: a mode of execution (which can be ci - core interpreter, ...), and ECMA-SL input file, and a monitor mode. And addition flag (--parse) can be added to parse the ECMA-SL file to a JSON file.

The following monitor modes are available:
	- ""      (No monitor)
	- "nsu"   (No-Sensitive-Upgrade monitor)

```
./main.native -mode <interpreter mode> -i <input file> -mon <monitor mode> [--parse]
```


## JS2ECMA-SL

A NodeJS tool that: transforms a JSON AST created using the [Esprima parser](https://esprima.org) in an ECMA-SL function that returns an ECMA-SL object containing the AST; Interpret ECMA-SL programs encoded in JSON AST (parsed by OCAML's interpreter); 

The AST refers to a JavaScript program that must be provided as a command-line argument.

This program is then passed to the Esprima parser to return the corresponding AST.

### Install

First, install all dependencies:

```
npm install
```

### Run Parser

This command-line tool expects as input a file containing a JS program.
Optionally, one can provide a file where to copy the resulting ECMA-SL function.

```
node src/index.js -i <input filename> -o <output filename>
```

A shorter version is provided as a `npm script`:

```
$INPUT=input.js npm start
```

This script expects that an environment variable named `INPUT` is available containing the name of the input file.
It also exports the ECMA-SL function directly to a file named `output.esl` created at the root of the project.

### Run Interpreter 

This command-line tool expects as input a file containing a ECMA-SL program encoded as a JSON AST and a monitor type:

```
node src/parse_esl.js <input filename> <monitor type>
```

A script is also provided to ease the comparison between the OCAML and NodeJS interpreters' results.
This script expects as input a monitor mode, a monitor mode (used only by OCAML's interpreter - use ci), and an .esl file or a directory containing .esl files, which is/are interpreted by the ocaml's .esl interpreter, followed by the parse to a JSON AST, finally, the JSON AST is interpreted by the NodeJS interpreter, where both results are compared.
An additional flag in provided (-d or --debug) that makes the script print auxiliary information about the programs execution.      

The following monitor modes are available:
	- ""      (No monitor)
	- "nsu"   (No-Sensitive-Upgrade monitor)

```
./autotest.sh (-i <input filename> || -pd <directory path> [-nd <directory path>]) -mon <monitor mode> -m <interpreter mode> [-d/--debug]
```



