function min(n1, n2) {
  return n1 <= n2 ? n1 : n2;
}

function mdc(n1, n2) {
  var mdc = 1;
  for (var i = 0; i <= min(n1, n2); i++)
    if (n1 % i == 0 && n2 % i == 0)
      mdc = i;
  return mdc;
}

mdc(20, 8);