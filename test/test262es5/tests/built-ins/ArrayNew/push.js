function assertEquals(a, b) {
  if (a === b) {
    return true;
  } else {
    throw 0;
  }
}

var arr = [1, 2]; 
arr.push(3);
assertEquals(arr[2], 2);