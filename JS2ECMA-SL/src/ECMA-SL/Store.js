const Lattice = require('./syntax/Lattice');
class Store{

constructor(params, vs){

	var result = {};
	params.forEach((param, i) => result[param]= vs[i]);
	this.sto = result;
	}
	getLvls(vars_arr){
		var reducer = (accumulator, value) => Lattice.lub(accumulator,value);
		var lvls = vars_arr.map(vs => vs.reduce(reducer, false));
		return lvls;
	}

	getLvl(vars){
		var arr = [];
		arr = arr.concat(vars);
		var reducer = (accumulator, value) => Lattice.lub(accumulator,value);
		var lvl = arr.reduce(reducer, false);
		return lvl;
	}

	getValues(vars_arr){
		var reducer = (accumulator, value) => Lattice.lub(accumulator,value);
		var value = vars_arr.map(vs => vs.reduce(reducer, 0));
		return value;
	}
}

module.exports=Store;
