function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_46.js
 * 
 * Simple regex test: no description
*/

let regex = /\s{2}\S?/;

let ret = regex.exec("\t 0");
AssertEquals(ret[0], "\t 0");

ret = regex.exec("\t a\t");
AssertEquals(ret[0], "\t a");

ret = regex.exec("\t ");
AssertEquals(ret[0], "\t ");

ret = regex.exec("  ");
AssertEquals(ret[0], "  ");

ret = regex.exec(" a");
AssertEquals(ret, null);