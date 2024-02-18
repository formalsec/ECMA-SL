function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_45.js
 * 
 * Simple regex test: no description
*/

let regex = /(?=(.*b))(a)(b)/;
let ret = regex.exec("abc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 4);
AssertEquals(ret[0], "ab");
AssertEquals(ret.index, 0);
