module.exports = {
  containsExpressions: paramsContainsExpression,
};


function paramsContainsExpression(params) {
  return params.some((param) => param.type == "AssignmentPattern")
};
