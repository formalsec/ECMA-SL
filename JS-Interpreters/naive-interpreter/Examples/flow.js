var x = 5, y = 0;

while (x > 0) {
	y = y * 10 + (x--);
	if (x == 3)
		continue;
	else if (x == 2)
		break;
}

for (var i = 0; i < 3; i++) {
	if (i == 1)
		continue;
	y = y * 10 + i;
}

y;
