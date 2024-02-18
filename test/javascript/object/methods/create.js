/**
 * * javascript/object/methods/create.js
 * 
 * Object.create - Creates an object that has the specified prototype or that 
 * has null prototype.
 * @return true
*/

let proto = { foo: 10, bar: "abc" };
let obj = Object.create(proto);

let x = (obj.foo === 10);
let y = (obj.bar === "abc");
AssertTrue(x && y);
