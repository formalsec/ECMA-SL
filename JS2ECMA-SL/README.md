# JS2ECMA-SL

A NodeJS tool that transforms a JSON AST created using the [Esprima parser](https://esprima.org) in an ECMA-SL function that returns an ECMA-SL object containing the AST.

The AST refers to a JavaScript program that must be provided as a command-line argument.

This program is then passed to the Esprima parser to return the corresponding AST.

## Run

First, install all dependencies:

```
npm install
```

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
