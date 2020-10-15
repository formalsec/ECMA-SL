module.exports = {
  transform: function(obj) {
    if (obj.type !== "Identifier") {
      throw Error('Unexpected object type; Expecting "Identifier"');
    }

    if (obj.name === "Infinity") {
      return {
        type: "Literal",
        value: Infinity,
        raw: "Infinity"
      }
    }

    if (obj.name === "NaN") {
      return {
        type: "Literal",
        value: NaN,
        raw: "NaN"
      }
    }

    if (obj.name === "undefined") {
      return {
        type: "Literal",
        value: undefined,
        raw: "undefined"
      }
    }

    return obj
  }
}
