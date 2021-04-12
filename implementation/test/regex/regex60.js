var __replaced = "abc".replace(/a/ig, 'd');

var __expected = 'dbc';

assert.sameValue(__replaced, __expected);