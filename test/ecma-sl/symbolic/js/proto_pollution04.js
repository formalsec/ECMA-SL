Object.sealProperties(Object.prototype);

function extend(target, obj) {
  Object.keys(obj).forEach(function (key) {
    src = target[key];
    val = obj[key];

    /* Prevents recursion */
    if (val === target) {
      return;
    } else if (typeof val !== 'object' || val === null) {
      target[key] = val;
      return;
    } else if (typeof src !== 'object' || src === null || Array.isArray(src)) {
      target[key] = extend({}, val);
      return;
    } else {
      target[key] = extend(src, val);
      return;
    }
  });
  return target
}

let obj1 = {};
let obj2 = { ['__proto__'] : { "polluted" : true } };
// let obj3 = { '__proto__' : { "polluted" : true } };
extend(obj1, obj2);
console.log(({}).polluted);
