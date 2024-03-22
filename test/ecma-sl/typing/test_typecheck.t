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
