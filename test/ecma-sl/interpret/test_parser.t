Test tuple parsing:
  $ ecma-sl interpret tuple.esl
  [1, 2., "string", symbol("undefined")]

Test extra semicolon parsing:
  $ ecma-sl interpret semicolon.esl
  2
