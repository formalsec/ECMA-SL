function is_palindrome(n) {

  function invert_number(n) {
    var res = 0;
    while (n > 0) {
      res = res * 10 + (n % 10);
      n = (n / 10) >> 0;
    }
    return res;
  }

  return n == invert_number(n);
}

is_palindrome(1234322)
is_palindrome(1234321)