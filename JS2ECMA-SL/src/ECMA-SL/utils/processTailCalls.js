const mapper = require("./mapper");

module.exports = {
  processTailCalls: ProcessTailCalls,
};


function ProcessTailCalls (obj) {


    function callback (obj) {
      switch (obj.type) {
        case "ReturnStatement": 
            if (obj.argument.type === "CallExpression") {
                obj.argument.is_tail_call = true;
            } else {
                obj.argument.is_tail_call = false;
            }

            return {
                obj, 
                recurse: true
            }

         default:
          return {
            obj,
            recurse: false
          }
      }
    }
  
    return mapper(callback, obj)
  }

