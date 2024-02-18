/**
 * * javascript/object/basic/access.js
 * 
 * Creates an empty object, initializes a fields with multiple data types and
 * retrieves their value.
 * @return true
*/

let obj = {};
obj.foo = 10;
obj.bar = "abc";
obj.baz = { qux: true };

let x = (obj.foo = 10);
let y = (obj.bar = "abc");
let z = (obj.baz.qux = true);
AssertTrue(x && y && z);
