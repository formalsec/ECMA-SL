// Copyright 2009 the Sputnik authors.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
info: |
    When the production "IfStatement: if ( Expression ) Statement else
    Statement" is evaluated, Statement(s) is(are) evaluated second
es5id: 12.5_A4
description: The first statement is "(function(){throw "instatement"})()"
---*/

//////////////////////////////////////////////////////////////////////////////
//CHECK#1
console.log("checkpoint0")
try {
	if (true) (function(){throw "instatement"})();
	console.log("Did not catch exception");
	throw new Test262Error("#1 failed") // se comentar ista linha funciona
} catch (e) {
	console.log("Caught Exception: " + e);
	if (e !== "instatement") {
		throw new Test262Error('#1: Exception === "instatement". Actual:  Exception ==='+ e);
	}
}
console.log("checkpoint1")
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//CHECK#2
try {
	if (false) (function(){throw "truebranch"})(); (function(){throw "missbranch"})();
	throw new Test262Error("#2 failed") // se comentar esta linha funciona
} catch (e) {
	if (e !== "missbranch") {
		throw new Test262Error('#2: Exception === "missbranch". Actual:  Exception ==='+ e);
	}
}
console.log("checkpoint2")
//
//////////////////////////////////////////////////////////////////////////////
