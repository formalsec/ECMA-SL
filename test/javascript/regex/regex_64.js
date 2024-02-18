function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_64.js
 * 
 * Simple regex test: no description
*/

let x;
let regex = new RegExp(x, "g");
let expected = /(?:)/g;
AssertEquals(regex.source, expected.source);

let str = String("asdf");
AssertEquals(str, "asdf");

let replaced = str.replace(regex, "1");
AssertEquals(replaced, "1a1s1d1f1");
