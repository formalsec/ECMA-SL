/**
 * * javascript/simple/primes.js
 * 
 * Function that checks if a number is prime. 
 * @return true
*/

function isPrime(value) {
	let primes = [];
	for (let i = 2; i <= value; i++) {
		primes[i] = true;
	}
	let limit = value;
	for (let i = 2; i <= limit; i++) {
		if (primes[i]) {
			for (let j = i * i; j <= value; j += i) {
				primes[j] = false;
			}
		}
	}
	return primes[value];
}

AssertEquals(isPrime(10), false);
AssertEquals(isPrime(23), true);
