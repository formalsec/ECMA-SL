var result = 0;

var i = 0;
while (i < 7) {
  result = result + 1
  i = i + 1
}
// result = 7


for (var i = 0; i < 3; i = i + 1) {
  result += 10; 
}
// result = 37


while (true) {
  result += 100
  break
  result += 100
}
// result = 137


for (var i = 0; i < 10; i++) {
  if (i % 2 == 0)
    continue;
  result += 1000
}


result;



// retval = 5137