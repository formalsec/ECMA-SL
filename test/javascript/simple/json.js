/**
 * * javascript/simple/json.js
 * 
 * Simple usage of the JSON parser over exponential numbers.
 * @return true
*/

let x = AssertEquals(JSON.parse('1e3'), 1000);
let y = AssertEquals(JSON.parse('1.0e3'), 1000);
let z = AssertEquals(JSON.parse('1.0E3'), 1000);
let w = AssertEquals(JSON.parse('1.0E+3'), 1000);

let k = (AssertEquals(JSON.parse('1e-3'), 0.001));

AssertTrue(x && y && z && w && k);
