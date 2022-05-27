// Copyright (C) 2020 Rick Waldron. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-promise.race
description: >
  Resolution the first resolved promise
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
---*/

let a = Promise.reject('a').catch(function (v) /* TODO: => */ {return v});
let b = Promise.resolve('b').then(function (v) /* TODO: => */ { throw v });
let c = Promise.reject('c').then(function (v) /* TODO: => */ { throw v; });
let d = Promise.resolve('d').finally(function (v) /* TODO: => */ {return v});
let e = Promise.reject('e').finally(function (v) /* TODO: => */ {return v});
let f = Promise.resolve('f').finally(function (v) /* TODO: => */ { throw v; });
let g = Promise.reject('g').finally(function (v) /* TODO: => */ { throw v; });
let h = Promise.reject('h').then(function (v) /* => */ {return v}, function () /* TODO: => */ {return 'j'});
let i = Promise.resolve('i').then(function (v) /* TODO: => */ {return v});

Promise.race([a, b, c, d, e, f, g, h, i]).then(function(winner) /* TODO: => */ {
  assert.sameValue(winner, 'a');
}).then($DONE, $DONE);
