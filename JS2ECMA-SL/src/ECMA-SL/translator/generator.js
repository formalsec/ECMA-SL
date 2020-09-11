function makeFreshVarGenerator(pref) {
  let count = 0;
  return () => pref + count++;
}

const generateFreshVar = makeFreshVarGenerator("__v");

module.exports = {
  generateFreshVar,
};
