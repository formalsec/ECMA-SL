Object.sealProperties(Object.prototype);

function merge(a, b) {
  for (var p in b) {
    try {
      if (b[p].constructor === Object) {
        a[p] = merge(a[p], b[p]);
      } else {
        a[p] = b[p];
      }
    } catch (e) {
      a[p] = b[p];
    }
  }
  return a;
}

let p = { constructor: { prototype: { status: "polluted" } } };
merge({}, p);
console.log(({}).status);
