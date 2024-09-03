// Copyright (C) 2016 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-%typedarray%.prototype.copywithin
description: Preservation of bit-level encoding
info: |
  Array.prototype.copyWithin (target, start [ , end ] )

  12. Repeat, while count > 0
    [...]
    d. If fromPresent is true, then
      i. Let fromVal be ? Get(O, fromKey).
      ii. Perform ? Set(O, toKey, fromVal, true).
includes: [nans.js, compareArray.js, testTypedArray.js]
features: [TypedArray]
---*/

function body(FloatArray) {
  var subject = new FloatArray(NaNs.length * 2);

  NaNs.forEach(function(v, i) {
    subject[i] = v;
  });

  var originalBytes, copiedBytes;
  var length = NaNs.length * FloatArray.BYTES_PER_ELEMENT;

  originalBytes = new Uint8Array(
    subject.buffer,
    0,
    length
  );
    /* *??* There are various types of NaN that have different bit encodings */
  console.log(subject.buffer) // [0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 255; 0; 0; 192; 255; 0; 0; 192; 127; 0; 0; 192; 255; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
  console.log(originalBytes) //   0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 255, 0, 0, 192, 255, 0, 0, 192, 127, 0, 0, 192, 255, 0, 0, 192, 127, 0, 0, 192, 127

  subject.copyWithin(NaNs.length, 0);
  copiedBytes = new Uint8Array(
    subject.buffer,
    length
  );

  console.log(subject.buffer) // [0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 255; 0; 0; 192; 255; 0; 0; 192; 127; 0; 0; 192; 255; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127; 0; 0; 192; 127]
  console.log(originalBytes)  //  0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 255, 0, 0, 192, 255, 0, 0, 192, 127, 0, 0, 192, 255, 0, 0, 192, 127, 0, 0, 192, 127
  console.log(copiedBytes)    //                                                                                                                                                  0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127, 0, 0, 192, 127

  assert(compareArray(originalBytes, copiedBytes));
}

testWithTypedArrayConstructors(body, [Float32Array]);
