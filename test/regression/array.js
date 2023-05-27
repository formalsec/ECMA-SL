// good
var arr1 = [ "x", "y" ]
console.log(arr1);
console.log(`cl.${ arr1 }()`)

var arr2 = [ "x", "y" ]
arr2.length = 2;
console.log(arr2);
console.log(`cl.${ arr2 }()`)

// bad
var arr3 = []
arr3.length = 2;
arr3.push("x");
arr3.push("y");
console.log(arr3);
console.log(`cl.${ arr3 }()`)
