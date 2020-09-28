class Store{

constructor(params, vs){

	var result = {};
	params.forEach((param, i) => result[param]= vs[i]);
	this.sto = result;
	}
}

module.exports=Store;
