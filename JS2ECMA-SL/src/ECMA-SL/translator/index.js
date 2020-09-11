const { generateFreshVar } = require("./generator");
const Transforms = require("../transforms");
const Assign = require("../syntax/Assign");
const NewObj = require("../syntax/NewObj");
const FieldAssign = require("../syntax/FieldAssign");
const NOpt = require("../syntax/NOpt");
const Val = require("../syntax/Val");
const Var = require("../syntax/Var");
const Return = require("../syntax/Return");
const Function = require("../syntax/Func");
const Block = require("../syntax/Block");

function translateLiteral(eslVal) {
  return {
    expression: new Val(eslVal),
    statements: [],
  };
}

function translateBoolean(value) {
  return translateLiteral(new Val.Bool(value));
}

function translateString(value) {
  return translateLiteral(new Val.Str(value));
}

function translateNull() {
  return translateLiteral(new Val.Null());
}

function translateNumber(value) {
  if (!Number.isFinite(value) || Number.isNaN(value)) {
    throw new Error("Invalid number: " + value);
  }
  if (Number.isInteger(value)) {
    if (Number.isSafeInteger(value)) {
      return translateLiteral(new Val.Int(value));
    }

    throw new Error("This number is not a safe integer: " + value);
  }
  return translateLiteral(new Val.Flt(value));
}

function translateArray(arr = []) {
  const varExpr = new Var(generateFreshVar());
  const exprsAndStmts = arr.map(traverseAndTranslate).reduce(
    (acc, exprStmts) => ({
      exprs: acc.exprs.concat(exprStmts.expression),
      stmts: acc.stmts.concat(exprStmts.statements),
    }),
    { exprs: [], stmts: [] }
  );

  return {
    expression: varExpr,
    statements: exprsAndStmts.stmts.concat(
      new Assign(varExpr, new NOpt(new NOpt.ListExpr(), exprsAndStmts.exprs))
    ),
  };
}

function translateObject(obj) {
  const varExpr = new Var(generateFreshVar());
  const newObjStmt = new Assign(varExpr, new NewObj());

  const objStmts = Object.keys(obj)
    .map((prop) => ({ prop, value: traverseAndTranslate(obj[prop]) }))
    .reduce(
      (acc, propValue) =>
        acc
          .concat(propValue.value.statements)
          .concat(
            new FieldAssign(varExpr, propValue.prop, propValue.value.expression)
          ),
      [newObjStmt]
    );

  return {
    expression: varExpr,
    statements: objStmts,
  };
}

function traverseAndTranslate(value) {
  switch (typeof value) {
    case "undefined":
      throw new Error("The undefined value is not supported");
    case "boolean":
      return translateBoolean(value);
    case "number":
      return translateNumber(value);
    case "string":
      return translateString(value);
    case "bigint":
      throw new Error("BigInt values are not supported: " + value);
    case "symbol":
      throw new Error("Symbol values are not supported: " + value);
    case "function":
      throw new Error("Functions are not supported: " + value);
    case "object":
      if (value === null) {
        return translateNull();
      } else if (value instanceof Array) {
        return translateArray(value);
      } else if (value instanceof RegExp) {
        throw new Error("Regular expressions are not supported: " + value);
      } else {
        const obj = Transforms.transformObject(value);
        return translateObject(obj);
      }

    default:
      throw new Error("Unexpected value: " + value);
  }
}

function fromJSObjectToESLStatements(objProg = {}) {
  const { expression, statements } = traverseAndTranslate(objProg);

  return statements.concat(new Return(expression));
}

function fromESLStatementsToESLFunction(
  name = "",
  params = [],
  statements = []
) {
  return new Function(name, params, new Block(statements));
}

module.exports = {
  fromJSObjectToESLStatements,
  fromESLStatementsToESLFunction,
};
