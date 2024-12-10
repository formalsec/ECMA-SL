Esl tests:
  $ ecma-sl symbolic assume.esl
  All Ok!
  $ ecma-sl symbolic extern.esl
  (#x : __$Int)
  unable to find external function 'i_dont_exist'
  [1]
  $ ecma-sl symbolic func.esl
  All Ok!
  $ ecma-sl symbolic if.esl
  All Ok!
  $ ecma-sl symbolic strings.esl
  All Ok!
  $ ecma-sl symbolic while.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_0.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_1.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_2.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_3.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_4.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_5.esl
  All Ok!
  $ ecma-sl symbolic object_lookup_6.esl
  All Ok!
