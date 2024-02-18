/**
 * * javascript/array/array_sort.js
 * 
 * Sorts a string (with the default sorted) and a numeric array (with a 
 * custom number sorter).
 * @return true
*/

const numberSorter = (x, y) => { return x - y };

let stringArray = [7, 10, 5, 2, 1];
let numberArray = [7, 10, 5, 2, 1];

stringArray.sort();
numberArray.sort(numberSorter);

let x = (CompareArrays(stringArray, [1, 10, 2, 5, 7]));
let y = (CompareArrays(numberArray, [1, 2, 5, 7, 10]));

AssertEquals(x && y, true);
