const { getParamsNames } = require("../utils/getParamsNames");

module.exports = {
  transform: function (obj) {
    
    if (obj.type !== "CatchClause") {
        throw Error('Unexpected object type; Expecting "CatchClause"');
    }


    const paramDetails = obj.param;
    //const paramsNames = getParamName(paramDetails);
    const paramsNames = getParamsNames([paramDetails]);
    obj.paramsNames = paramsNames;
    /*
    obj.body.isSimpleParameterList = isSimpleParameterList(obj.params);
    obj.body.variableDeclarations = variableDeclarations;
    obj.body.functionDeclarations = functionDeclarations;
    obj.body.letDeclarations = letDeclarations;
    obj.body.containsExpression = containsExpressions(obj.params);
    obj.body.paramsDetails = paramsDetails;
    obj.body.paramsNames = paramsNames;

    obj.params = paramsNames;

    obj.body.codeType = "function";

    obj.body.strict = hasStrictDirective(obj.body.body);
    */
    return obj;
  },
};