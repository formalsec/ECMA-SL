var a = new Int32Array(1);

a[0] = 1; // this assingment uses the ES5 PutValue which uses [[Put]] instead of [[Set]]

asserts.sameValue(a[0], 1);