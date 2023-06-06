// Summary: process.argv
var process = {};
process.argv = new Proxy([],
	(function () {
		var tbl = {};
		var len = 0;
		return {
			get(a, i) {
				if (i == "length") return len;
				if (!tbl[i]) { 
					var x = esl_symbolic.string("argv_" + i);
					tbl[i] = x;
					if (i >= len) len = i + 1;
				}
				return tbl[i];
			}
		}
	})()
)


// Test15
const v1 = process.argv;
const x = v1[2];
const v2 = esl_symbolic.evalWrapper(x);

