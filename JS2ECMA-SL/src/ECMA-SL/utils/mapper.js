module.exports = mapper;

function mapper(callback, obj) {
  if (!obj) return obj;

  var ret = callback(obj);
  var new_obj = ret.obj;
  if (!ret.recurse) return new_obj;

  switch (new_obj.type) {
    //
    // Scripts
    //
    case "Program":
      return {
        type: "Program",
        body: new_obj.body.map((obj) => mapper(callback, obj)),
      };

    //
    // Expressions
    //
    case "ArrayExpression":
      return {
        type: "ArrayExpression",
        elements: new_obj.elements.map((obj) => mapper(callback, obj)),
      };

    case "ObjectExpression":
      return {
        type: "ObjectExpression",
        properties: new_obj.properties.map((obj) => mapper(callback, obj)),
      };

    case "Property":
      return {
        type: "Property",
        key: mapper(callback, new_obj.key),
        value: mapper(callback, new_obj.value),
        computed: new_obj.computed,
        kind: new_obj.kind,
        shorthand: new_obj.shorthand,
      };

    case "MemberExpression":
      return {
        type: "MemberExpression",
        object: mapper(callback, new_obj.object),
        property: mapper(callback, new_obj.property),
        computed: new_object.computed,
      };

    case "CallExpression":
    case "NewExpression": {
      const resultCallee = traverse(callback, obj.callee);
      const resultArguments = mapReduce(obj.arguments);

      resultData = resultCallee.data.concat(resultArguments);
      break;
    }

    case "UpdateExpression":
    case "UnaryExpression":
      resultData = traverse(callback, obj.argument).data;
      break;

    case "BinaryExpression":
    case "LogicalExpression":
    case "AssignmentExpression": {
      const resultLeft = traverse(callback, obj.left);
      const resultRight = traverse(callback, obj.right);

      resultData = resultLeft.data.concat(resultRight.data);
      break;
    }

    case "SequenceExpression":
      resultData = mapReduce(obj.expressions);
      break;

    //
    // Statements and Declarations
    //
    case "BlockStatement":
      resultData = mapReduce(obj.body);
      break;

    case "DoWhileStatement":
    case "WhileStatement": {
      const resultTest = traverse(callback, obj.test);
      const resultBody = traverse(callback, obj.body);

      resultData = resultTest.data.concat(resultBody.data);
      break;
    }

    case "ExpressionStatement":
      resultData = traverse(callback, obj.expression).data;
      break;

    case "ForStatement": {
      const resultInit = traverse(callback, obj.init);
      const resultTest = traverse(callback, obj.test);
      const resultUpdate = traverse(callback, obj.update);
      const resultBody = traverse(callback, obj.body);

      resultData = resultInit.data.concat(
        resultTest.data,
        resultUpdate.data,
        resultBody.data
      );
      break;
    }

    case "ForInStatement": {
      const resultLeft = traverse(callback, obj.left);
      const resultRight = traverse(callback, obj.right);
      const resultBody = traverse(callback, obj.body);

      resultData = resultLeft.data.concat(resultRight.data, resultBody.data);
      break;
    }

    case "FunctionDeclaration":
    case "FunctionExpression":
    case "LabeledStatement":
      resultData = traverse(callback, obj.body).data;
      break;

    case "IfStatement":
    case "ConditionalExpression": {
      const resultTest = traverse(callback, obj.test);
      const resultConsequent = traverse(callback, obj.consequent);
      const resultAlternate = traverse(callback, obj.alternate);

      resultData = resultTest.data.concat(
        resultConsequent.data,
        resultAlternate.data
      );
      break;
    }

    case "ReturnStatement":
    case "ThrowStatement":
      resultData = traverse(callback, obj.argument).data;
      break;

    case "SwitchStatement": {
      const resultDiscriminant = traverse(callback, obj.discriminant);
      const resultCases = mapReduce(obj.cases);

      resultData = resultDiscriminant.data.concat(resultCases);
      break;
    }
    case "SwitchCase": {
      const resultTest = traverse(callback, obj.test);
      const resultConsequent = mapReduce(obj.consequent);

      resultData = resultTest.data.concat(resultConsequent);
      break;
    }

    case "VariableDeclaration":
      resultData = mapReduce(obj.declarations);
      break;
    case "VariableDeclarator": {
      const resultId = traverse(callback, obj.id);
      const resultInit = traverse(callback, obj.init);

      resultData = resultId.data.concat(resultInit.data);
      break;
    }

    case "WithStatement": {
      const resultObject = traverse(callback, obj.object);
      const resultBody = traverse(callback, obj.body);

      resultData = resultObject.data.concat(resultBody.data);
      break;
    }

    case "TryStatement":
      throw Error("Traverse for TryStatements is not implemented!");

    default:
      resultData = [];
      break;
  }

  return {
    data: cbResult.data.concat(resultData),
  };
}
