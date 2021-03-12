const regexpTree = require("regexp-tree");
const mapper = require("./mapper");

function parseRegExps(obj) {
  function callback(obj) {
    if (!obj)
      return {
        obj,
        recurse: false,
      };

    switch (obj.type) {
      case "Literal":
        if (obj.hasOwnProperty("regex")) {
          return {
            obj: {
              type: "Literal",
              value: obj.raw,
              raw: obj.raw,
              regex: regexpTree.parse(obj.raw),
            },
            recurse: false,
          };
        }

        return {
          obj,
          recurse: false,
        };

      default:
        return {
          obj,
          recurse: true,
        };
    }
  }

  return mapper(callback, obj);
}

module.exports = parseRegExps;
