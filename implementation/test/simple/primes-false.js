/**
  @return #f
*/

function isPrime(value) {
  var primes = [];
  for(var i = 2; i <= value; i++) {
    primes[i] = true;
  }
  var limit = value;
  for(var i = 2; i <= limit; i++) {
    if(primes[i] === true) {
      for(var j = i * i; j <= value; j += i) {
        primes[j] = false;
      }
    }
  }
  return primes[value];
}

var n = 4;

var ret1 = isPrime(n);

Assert(ret1 = false)