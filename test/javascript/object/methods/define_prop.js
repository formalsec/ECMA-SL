/**
 * * javascript/object/methods/define_prop.js
 * 
 * Object.defineProperties - Adds one or more properties to an object, and/or 
 * modifies attributes of existing properties.
 * @return true
*/

let obj = { foo: 10 };

Object.defineProperties(obj, {
  "foo": { value: 20, writable: true },
  "bar": { value: "abc", writable: false }
});

let x = (obj.foo == 20);
let y = (obj.bar == "abc");
AssertTrue(x && y);
