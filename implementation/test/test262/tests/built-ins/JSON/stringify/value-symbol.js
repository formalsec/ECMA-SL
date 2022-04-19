// Copyright (C) 2019 Aleksey Shvayka. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
esid: sec-serializejsonproperty
description: >
  Symbol primitives are ignored, both as keys and as values.
info: |
  JSON.stringify ( value [ , replacer [ , space ] ] )

  [...]
  12. Return ? SerializeJSONProperty(the empty String, wrapper).

  SerializeJSONProperty ( key, holder )

  [...]
  11. Return undefined.
features: [Symbol]
---*/

var sym = Symbol('desc');
assert.sameValue(JSON.stringify(sym), undefined);
console.log("Checkpoint1")
assert.sameValue(JSON.stringify([sym]), '[null]');
console.log("Checkpoint2")
assert.sameValue(JSON.stringify({key: sym}), '{}');
console.log("Checkpoint3")
var obj = {};
obj[sym] = 1;
console.log(JSON.stringify(obj)) // -> {\"Symbol(desc)\":1}
assert.sameValue(JSON.stringify(obj), '{}');
