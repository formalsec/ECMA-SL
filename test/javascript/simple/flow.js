/**
 * * javascript/simple/flow.js
 * 
 * Loop statement that produces the sums all even numbers from 1 to 1000.
*/

let total = 0;
for (let i = 0; i < 1000; i++) {
	if (i % 2 == 0) {
		total += i;
	}
}

console.log("Total: " + total);
AssertEquals(total, 249500);
