let esl_symbolic = require("esl_symbolic");

Object.sealProperties(Object.prototype);

function merge(a, b) {
  for (const p in b) {
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

let polluted = esl_symbolic.polluted_object();
merge({}, polluted);
console.log(({}).status);
