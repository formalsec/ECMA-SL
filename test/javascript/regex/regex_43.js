function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_43.js
 * 
 * Simple regex test: no description
*/

let regex = /\n\v\f\r\t\0/;
let ret = regex.exec("ccab\n\v\f\r\t\0cc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], "\n\v\f\r\t\0");
AssertEquals(ret.index, 4);
AssertEquals(ret.input, "ccab\n\v\f\r\t\0cc");

regex = /[\n]+/;
ret = regex.exec("ccab\n\n\ncc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], "\n\n\n");
AssertEquals(ret.index, 4);
AssertEquals(ret.input, "ccab\n\n\ncc");