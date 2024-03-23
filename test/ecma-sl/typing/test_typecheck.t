Run ecma-sl type checking tests:
  $ find examples/** -name "*.esl" -exec ecma-sl compile {} -o /dev/null \;
  
  TypeError: Value of type '"abc"' cannot be returned by a 'int' function.
  File "return.esl", line 11, characters 47-48
  11 |   function badValueReturn(): int 			{ return "abc" };		/* BadReturn: int <- string */
                                        			             ^
  
  TypeError: Value of type 'void' cannot be returned by a 'int' function.
  File "return.esl", line 7, characters 35-41
  7 |   function badVoidReturn(): int 			{ return };					/* BadReturn: int <- void */
                                      			  ^^^^^^
  
  TypeError: Value of type '10' cannot be returned by a 'void' function.
  File "return.esl", line 12, characters 45-47
  12 |   function badValueVoidReturn(): void { return 10 }				/* BadReturn: void <- int */
                                                      ^^
  
  TypeError: Value of type 'string' is not assignable to type 'int'.
  File "assign.esl", line 6, characters 30-31
  6 |   badTypedVariable: int := "abc";												/* BadValue: int <- string */
                                     ^
  
  TypeError: Value of type 'undefined' is not assignable to type 'symbol'.
  File "assign.esl", line 10, characters 36-46
  10 |   badUndefinedSymVariable: symbol := 'undefined; 				/* BadValue: symbol <- undefined */
                                            ^^^^^^^^^^
  
  TypeError: Value of type '20' is not assignable to type '10'.
  File "assign.esl", line 14, characters 30-32
  14 |   badLiteralVariableVal: 10 := 20;											/* BadValue: 10 <- 20 */
                                      ^^
  
  TypeError: Value of type '"abc"' is not assignable to type '10'.
  File "assign.esl", line 15, characters 35-36
  15 |   badLiteralVariableType: 10 := "abc";									/* BadValue: 10 <- "abc" */
                                           ^
  
  TypeError: Value of type 'int' is not assignable to type 'string'.
  File "assign.esl", line 20, characters 37-51
  20 |   untypedBadVarPropagation: string := baseUntypedVar;		/* BadValue: string <- int */
                                             ^^^^^^^^^^^^^^
  
  TypeError: Value of type 'int' is not assignable to type 'string'.
  File "assign.esl", line 25, characters 35-47
  25 |   typedBadVarPropagation: string := baseTypedVar;				/* BadValue: string <- int */
                                           ^^^^^^^^^^^^
