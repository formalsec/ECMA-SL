function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_69.js
 * 
 * Simple regex test: hexadecimal
*/

let str = '\x41';
let str2 = 'A';
AssertEquals(str, str2);

let regex = /\x41/;
let ret = regex.test("A");
AssertEquals(ret, true);

regex = /[\x41]/;
ret = regex.test("A");
AssertEquals(ret, true);
AssertEquals(String.fromCharCode(0x0041), "A");

str = "\xe0\xa4\xb1";
str2 = "à¤±";
AssertEquals(str, str2);

regex = /[\xe0\xa4\xb1]/;
ret = regex.test("à¤±");
AssertEquals(ret, true);
