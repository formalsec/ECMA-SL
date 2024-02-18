function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_70.js
 * 
 * Simple regex test: octal
*/

let str = '\101';
let str2 = 'A';
AssertEquals(str, str2);

let regex = /\101/;
let ret = regex.test("A");
AssertEquals(ret, true);

regex = /[\101]/;
ret = regex.test("A");
AssertEquals(ret, true);
