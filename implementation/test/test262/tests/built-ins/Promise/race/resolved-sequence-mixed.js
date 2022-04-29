// Copyright (C) 2020 Rick Waldron. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-promise.race
description: >
  Resolution ticks are set in a predictable sequence of mixed fulfilled and rejected promises
info: |
  PerformPromiseRace

  Repeat,
    Let next be IteratorStep(iteratorRecord).
    If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
    ReturnIfAbrupt(next).
    If next is false, then
      Set iteratorRecord.[[Done]] to true.
      Return resultCapability.[[Promise]].
    Let nextValue be IteratorValue(next).
    If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
    ReturnIfAbrupt(nextValue).
    Let nextPromise be ? Call(promiseResolve, constructor, « nextValue »).
    Perform ? Invoke(nextPromise, "then", « resultCapability.[[Resolve]], resultCapability.[[Reject]] »).

flags: [async]
includes: [promiseHelper.js]
---*/

let a = Promise.reject('');
let b = new Promise(function (resolve) /* TODO: => */ {return resolve('')});
let c = new Promise(function (_, reject) /* TODO: => */ {return reject('')});
let sequence = [1];
Promise.all([
  a.catch(function () /* TODO: => */ {
    sequence.push(3);
  }),
  Promise.race([a, b, c]).then(function () /* TODO: => */ {
    // This should not be present when the final
    // sequence is evaluated.
    sequence.push(5);
  }),
  b.then(function () /* TODO: => */ {
    sequence.push(4);
  }),
]).catch(function () /* TODO: => */ {
  assert.sameValue(sequence.length, 4);
  checkSequence(sequence);
}).then($DONE, $DONE);
sequence.push(2);
