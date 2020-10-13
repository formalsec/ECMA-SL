var a = 1;
var b = 2;
var c = 3;
var d = 4;
var obj = {
  p1: "to delete",
  p2: "keep this",
};

a++;
b--;
++c;
--d;

var e = +!false === -(-1);

var f1 = typeof undefined,
  f2 = typeof null,
  f3 = typeof false,
  f4 = typeof d,
  f5 = typeof "",
  f6 = typeof obj;

delete obj.p1;

var g = ~d;

var h1 = 9 << 3, /* 72 */
    h2 = 9 >> 2, /* 2 */
    h3 = -9 >> 2, /* -3" */
    h4 = 9 >>> 2, /* 2 */
    h5 = -9 >>> 2; /* 1073741821 */

var i1 = 1 | 0,
    i2 = 0 | 0,
    i3 = 1 | 1;

var j1 = i1 === i3 ? "correct" : "incorrect",
    j2 = i1 === i2 ? "incorrect" : "correct";
