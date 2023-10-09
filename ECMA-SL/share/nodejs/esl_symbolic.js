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
function is_symbolic(_x) { }
function lazy_object() { return {}; }

module.exports.any = function(_x) { return {}; };
module.exports.number = get;
module.exports.string = get;
module.exports.boolean = get;
module.exports.function = function(_x) { return function() {}; }
module.exports.lazy_object = lazy_object
module.exports.assume = ignore;
module.exports.assert = ignore;
module.exports.is_symbolic = is_symbolic;
module.exports.evalWrapper = eval;
module.exports.execWrapper = require('child_process').exec;
module.exports.execSyncWrapper = require('child_process').execSync;
