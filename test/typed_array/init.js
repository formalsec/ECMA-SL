var a = new Int32Array();

var b = new Int32Array(1);

var c = new Int32Array(a); // comparison of DataBlocks is failing section_6.2.esl:41 

var d = new Int32Array([1, 2, 3]); // missing list remove operation for TypedArrayFrom 

var e = new Int32Array([1, 2, 3], 1, 1); // missing list remove operation for TypedArrayFrom 