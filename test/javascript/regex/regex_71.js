function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_71.js
 * 
 * Simple regex test: unicode
*/

let str = '\u0041';
let str2 = 'A';
AssertEquals(str, str2);

let regex = /\u0041/;
let ret = regex.test("A");
AssertEquals(ret, true);

regex = /[\u0041]/;
ret = regex.test("A");
AssertEquals(ret, true);

str = '\u0931';
str2 = "ऱ";
AssertEquals(str, str2);

regex = /[\u0931]/;
ret = regex.test("ऱ");
AssertEquals(ret, true);
