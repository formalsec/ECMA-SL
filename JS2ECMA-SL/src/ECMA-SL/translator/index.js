const { generateFreshVar } = require("./generator");
const Transforms = require("../transforms");
const Stmt = require("../syntax/Stmt/Stmt");
const Assign = require("../syntax/Stmt/Assign")(Stmt);
const FieldAssign = require("../syntax/Stmt/FieldAssign")(Stmt);
const ExprModule = require("../syntax/Expr/Expr");
const NewObj = ExprModule.NewObj;
const NOpt = ExprModule.NOpt;
const Var = ExprModule.Var;
const ValExpr = ExprModule.Val;
const ValModule = require("../syntax/Val/Val");
const Val = ValModule.Val;
const PrimitiveVal = ValModule.PrimitiveVal;
const Return = require("../syntax/Stmt/Return")(Stmt);
const Function = require("../syntax/Func");
const Block = require("../syntax/Stmt/Block")(Stmt);
const SymbolVal = require("../syntax/Val/SymbolVal")(Stmt);

function translateLiteral(eslVal) {
  return {
    expression: new ValExpr(new Val(eslVal)),
    statements: [],
  };
}

function translateBoolean(value) {
  return translateLiteral(new PrimitiveVal(value));
}

function translateString(value) {
  return translateLiteral(new PrimitiveVal(value));
}

function translateNull() {
  return translateLiteral(new PrimitiveVal(null));
}

function translateUndefined() {
  return translateLiteral(new SymbolVal("undefined"));
}

function translateNumber(value) {
  return translateLiteral(new PrimitiveVal(value));
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
    .map((prop) => ({ prop: translateString(prop), value: traverseAndTranslate(obj[prop]) }))
    .reduce(
      (acc, propValue) =>
        acc
          .concat(propValue.value.statements)
          .concat(
            new FieldAssign(varExpr, propValue.prop.expression, propValue.value.expression)
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
      return translateUndefined();
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
