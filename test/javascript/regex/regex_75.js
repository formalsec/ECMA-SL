function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_75.js
 * 
 * Simple regex test: no description
*/

let ret = /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac");

AssertEquals(ret[0], "zaacbbbcac");
AssertEquals(ret[1], "z");
AssertEquals(ret[2], "ac");
AssertEquals(ret[3], "a");
AssertEquals(ret[4], undefined);
AssertEquals(ret[5], "c");
