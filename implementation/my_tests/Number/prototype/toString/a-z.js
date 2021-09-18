// Copyright (C) 2020 Rick Waldron. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-number.prototype.tostring
description: >
  Letters a-z are used for digits with values 10 through 35
info: |
  6. Return the String representation of this Number value using
  the radix specified by radixNumber. Letters a-z are used for
  digits with values 10 through 35. The precise algorithm is
  implementation-dependent, however the algorithm should be a
  generalization of that specified in 6.1.6.1.20.
---*/

// O código está a cair no último return da 
// funcão e não temos o algoritmo com o radix imoplementado

for (let radix = 11; radix <= 36; radix++) {
  for (let i = 10; i < radix; i++) {
    console.log(i.toString(radix) + ", " + String.fromCharCode(i + 87));
    assert.sameValue(i.toString(radix), String.fromCharCode(i + 87));
  }
}
