var x;

var __instance = new Boolean;

__instance.replace = String.prototype.replace;

console.log("I AM HERE");
console.log(__instance.replace);

var ret = __instance.replace(function() {
    return false;
  }(), x);

console.log(ret);

assert.sameValue(ret, 'undefined');