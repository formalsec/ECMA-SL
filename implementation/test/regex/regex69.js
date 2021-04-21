var ret = "anullbc".replace(null, "d");

assert.sameValue(ret, "adbc");