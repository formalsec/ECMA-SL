// calculates the factorial of n
function factorial(n) {
  if (n == 0)
    return 1;

  return n * factorial(n - 1);
}

factorial(5);



// retval = 120