var iface___instr_symb_str_0 = "CDE; $(touch success) #FG";
var callback___instr_symb_0 = function () { };

const v1 = require('child_process');
var exec = v1.exec;
const v10 = function (iface, callback) {
	const v2 = 'cat /sys/class/net/' + iface;
	const v3 = v2 + '/address';
	const v8 = function (err, out) {
		if (err) {
			const v4 = callback(err, null);
			v4;
			return;
		}
		const v5 = out.trim();
		const v6 = v5.toLowerCase();
		const v7 = callback(null, v6);
		v7;
	};
	const v9 = exec(v3, v8);
	v9;
};

v10(iface___instr_symb_str_0, callback___instr_symb_0);
