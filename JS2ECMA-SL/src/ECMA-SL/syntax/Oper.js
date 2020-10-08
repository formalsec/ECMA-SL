
const Val = require("./Val/Val");
const PrimitiveVal = require("./Val/PrimitiveVal")(Val);
class Oper{
  constructor(operator, type) {
    this.operator = operator;
    this.type = type;
  }
  interpret(val1,val2){
  	switch(this.operator){
  		//BinOpt
  		case "Plus": return new PrimitiveVal(val1.value + val2.value);
  		case "Minus": return new PrimitiveVal(val1.value + val2.value);
  		case "Times": return new PrimitiveVal(val1.value * val2.value);
  		case "Div": return new PrimitiveVal(val1.value - val2.value);
  		case "Equal": return new PrimitiveVal(val1.value = val2.value); //Check
  		case "Gt": return new PrimitiveVal(val1.value > val2.value); //Check
  		case "Lt": return new PrimitiveVal(val1.value < val2.value); //Check
  		case "Egt": return new PrimitiveVal(val1.value >= val2.value); //Check
  		case "Elt": return new PrimitiveVal(val1.value <= val2.value); //Check
  		case "Log_And": return new PrimitiveVal(val1.value && val2.value); //Check
  		case "Log_Or": return new PrimitiveVal(val1.value || val2.value); //Check
  		case "InObj": return new PrimitiveVal(true); //TODO //Extended ECMA-SL
  		case "InList": return new PrimitiveVal(val1.value.includes(val2.value));
  		case "Lnth": return new PrimitiveVal(true);//TODO
  		case "Tnth":return new PrimitiveVal(true);//TODO
  		case "Ladd":return new PrimitiveVal(true);//TODO
  		case "Lconcat": return new ListVal(val1.value.concat(val2.value))
  		//UnOpt
  		case "Neg": return new PrimitiveVal(-val1.value);
  		case "Not": return new PrimitiveVal(!val1.value);
  		case "Typeof": return new PrimitiveVal(typeof val1.value);
  		case "ListLen": return new PrimitiveVal(val1.value.length);
  		case "TupleLen": return new PrimitiveVal(val1.value.length); // JS does not have tuples
  		case "Head": return val1.getMember(0);
  		case "Tail": return val1.getTail();
  		case "First": return new Val(val1.value[0]); //JS does not have tuples
  		case "Second": return new Val(val1.value.slice(1)); // JS does not have tuples
  		case "IntToFloat": return new PrimitiveVal(0.0 + val1.value);
  		case "FloatToString": return new PrimitiveVal(String.valueOf(val1.value));
  		case "ObjToList": return new ListVal([]);//TODO
  		//NOpt
  		case "ListExpr":
  		case "TupleExpr":
  		case "NAry_And": var reducer = (accumulator, value) => accumulator && value; 
  						 return new PrimitiveVal(val1.value.reduce(reducer));
  		case "NAry_Or": var reducer = (accumulator, value) => accumulator || value; 
  						return new PrimitiveVal(val1.value.reduce(reducer));
  		default: throw new Error("Unsupported Argument")
  	}

  }
}

Oper.fromJSON = function(obj){
  return new Oper(obj.value, obj.type);
}

module.exports = Oper;