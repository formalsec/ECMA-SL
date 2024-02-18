function Assert(cond) { if (cond !== true) { throw new Error("Assertion failed!"); } } function AssertUnreachable() { throw new Error("Assertion failed!"); } function AssertEquals(val, exp) { return Assert(val === exp); } function AssertArray(arr, exp) { return Assert(arr.length === exp.length && arr.every((element, index) => element === exp[index])); } function AssertObject(obj, exp) { return Assert(Object.keys(obj).length === Object.keys(exp).length && Object.keys(obj).every((fld, _) => obj[fld] === exp[fld])) }
/**
 * * javascript/regex/regex_72.js
 * 
 * Simple regex test: no description
*/

AssertEquals("\xFF", "\u00FF");

let arr = /\xFF/.exec("\u00FF");
AssertEquals(arr[0], "\u00FF");

arr = /\u00FF/.exec("\xFF");
AssertEquals(arr[0], "\u00FF");

arr = /\u00FF/.exec("\u00FF");
AssertEquals(arr[0], "\u00FF");

arr = /[\xFF]/.exec("\u00FF");
AssertEquals(arr[0], "\u00FF");

arr = /[\u00FF]/.exec("\xFF");
AssertEquals(arr[0], "\u00FF");

let ret = String.fromCharCode(255);
AssertEquals(ret, "\u00FF");
AssertEquals(ret, "\xFF");
