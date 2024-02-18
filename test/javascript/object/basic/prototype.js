/**
 * * javascript/object/basic/prototype.js
 * 
 * Simple object with a basic prototype inheritance.
 * @return true
*/

let proto = { foo: 10, bar: "abc" };
let obj = {};

Object.setPrototypeOf(obj, proto);

let x = (obj.foo === 10);
let y = (obj.bar === "abc");
AssertTrue(x && y);
