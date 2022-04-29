// Copyright (C) 2020 Rick Waldron. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-promise.any
description: >
  Promise.any resolves with the first item that does not reject.
flags: [async]
features: [Promise.any, arrow-function]
---*/

let fulfillables = [
  Promise.reject('a'),
  new Promise(function (resolve, reject) /* TODO: => */ {return reject('b')}),
  Promise.all([Promise.reject('c')]),
  Promise.reject('d').catch(function (v) /* TODO: => */ {return v}),
];

Promise.any(fulfillables)
  .then(function (resolution) /* TODO: => */ {
    assert.sameValue(resolution, 'd');
  }).then($DONE, $DONE);
