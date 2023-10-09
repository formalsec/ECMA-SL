const esl_symbolic = require('esl_symbolic');

function symbolic_string_array(prefix, n) {
  var arr = [];
	for (let i = 0; i < n; ++i) {
		arr.push(esl_symbolic.string(prefix + "_" + i));
	}
	return arr;
}
var color___instr_symb_str_0 = symbolic_string_array("color", 2)

//const chalk = require('chalk');
//const cl = require('chalkline');
//const Table = require('cli-table');
//const pkgInfo = require('../package.json');
const CLI_ICON_FAIL = '\u2718';
const CLI_ICON_PASS = '\u2713';
const CLI_ICON_WARN = '\u267A ';
const CLI_ICON_NOTE = '\u270F︎ ';
const v2 = () => {
	const v1 = pkgInfo.version;
	return v1;
};
const v4 = () => {
	const v3 = pkgInfo.name;
	return v3;
};
const v6 = (...params) => {
	const v5 = console.log(...params);
	v5;
	return params;
};
const v10 = (...params) => {
	const v7 = chalk.cyan;
	const v8 = v7.bold(CLI_ICON_NOTE, ...params);
	const v9 = console.log(v8);
	v9;
	return params;
};
const v12 = (msg, ...params) => {
	const v11 = console.log(msg, ...params);
	v11;
	return params;
};
const v16 = (...params) => {
	const v13 = chalk.green;
	const v14 = v13.bold(CLI_ICON_PASS, ...params);
	const v15 = console.log(v14);
	v15;
	return params;
};
const v20 = (...params) => {
	const v17 = chalk.yellow;
	const v18 = v17.bold(CLI_ICON_WARN, ...params);
	const v19 = console.log(v18);
	v19;
	return params;
};
const v24 = (...params) => {
	const v21 = chalk.red;
	const v22 = v21.bold(CLI_ICON_FAIL, ...params);
	const v23 = console.log(v22);
	v23;
	return params;
};
const v43 = data => {
	let table;
	let head = [];
	const v25 = data.length;
	const v26 = v25 > 0;
	if (v26) {
		const v27 = data[0];
		const v28 = Array.isArray(v27);
		if (v28) {
			header = data[0];
			const v29 = data.splice(0, 1);
			v29;
		} else {
			const v30 = data[0];
			header = Object.keys(v30);
		}
		const v33 = function (item) {
			const v31 = chalk.cyan;
			const v32 = v31.bold(item);
			return v32;
		};
		header = header.map(v33);
		const v34 = { head: header };
		table = new Table(v34);
		const v39 = item => {
			const v35 = Object.keys(item);
			const v37 = key => {
				const v36 = item[key];
				return v36;
			};
			let values = v35.map(v37);
			const v38 = table.push(values);
			v38;
		};
		const v40 = data.map(v39);
		v40;
		const v41 = table.toString();
		const v42 = console.log(v41);
		v42;
	}
};
const v52 = color => {
	const v44 = color.length;
	const v45 = v44 > 0;
	if (v45) {
		try {
			console.log(color);
			const v46 = `cl.${ color }()`;
			console.log(v46);
			const v47 = esl_symbolic.evalWrapper(v46);
			v47;
		} catch (e) {
			const v48 = chalk.bgRed;
			const v49 = `Invalid Color: ${ color }`;
			const v50 = v48.bold(v49);
			const v51 = console.error(v50);
			v51;
		}
	}
};
const v54 = data => {
	const v53 = console.dir(data);
	v53;
	return data;
};
const messenger = {};
messenger.version = v2;
messenger.name = v4;
messenger.log = v6;
messenger.info = v10;
messenger.note = v12;
messenger.success = v16;
messenger.warning = v20;
messenger.error = v24;
messenger.table = v43;
messenger.line = v52;
messenger.dir = v54;

v52(color___instr_symb_str_0);
