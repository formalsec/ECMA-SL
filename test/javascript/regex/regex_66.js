function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_66.js
 * 
 * Simple regex test: no description
*/

let str = 'She sells seashells by the seashore.';
let regex = /sh/g;
let ret = str.replace(regex, "$$" + 'sch');

AssertEquals(ret, 'She sells sea$schells by the sea$schore.');
