/**
 * * javascript/simple/json.js
 * 
 * Simple usage of the JSON parser over exponential numbers.
 * @return true
*/

AssertEquals(JSON.parse('1e3'), 1000);
AssertEquals(JSON.parse('1.0e3'), 1000);
AssertEquals(JSON.parse('1.0E3'), 1000);
AssertEquals(JSON.parse('1.0E+3'), 1000);
AssertEquals(JSON.parse('1e-3'), 0.001);
