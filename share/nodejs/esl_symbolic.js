const fs = require('fs');

const witness = process.argv[2];
if (!fs.existsSync(witness)) {
  console.log(`Non-existent witness file '${witness}'`);
  process.exit(1);
}

const symbolic_map = require(witness).symbolic_map;
if (symbolic_map === undefined) {
  console.log(`Unable to load symbolic_map from '${witness}'`)
  process.exit(1);
}

function get(x) { return symbolic_map[x]; }

function ignore(_x) { }

function is_symbolic(_x) { return false }

function lazy_object() { return {}; }

function polluted_object(depth) {
  switch (depth) {
    case 2:
      return { ["__proto__"]: { "toString": "polluted" } }
    case 3:
      return { "constructor": { "prototype": { "toString": "polluted" } } }
    default:
  }
  return {}
}

module.exports = {
  any: get,
  number: get,
  string: get,
  boolean: get,
  function: function(_x) { return function() { }; },
  lazy_object: lazy_object,
  assume: ignore,
  assert: ignore,
  is_symbolic: is_symbolic,
  sealProperties: ignore,
  polluted_object: polluted_object
};
