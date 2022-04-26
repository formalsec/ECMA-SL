// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: Array.sort should not eat exceptions
esid: sec-array.prototype.sort
description: comparefn function throw "error"
---*/

/* assert.throws(Test262Error, () => { TODO: uncomment and remove call below when arrow functions are implemented
  var myComparefn = function(x, y) {
    throw new Test262Error();
  }
  var x = [1, 0];
  x.sort(myComparefn);
}); */

assert.throws(Test262Error, function () {
  var myComparefn = function(x, y) {
    throw new Test262Error();
  }
  var x = [1, 0];
  x.sort(myComparefn);
});
