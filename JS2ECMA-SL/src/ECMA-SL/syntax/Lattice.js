/*
	true -> High
	false -> Low
*/

function lub(lev1, lev2){
	return (lev1 && lev2);
}
function lubn(lev_arr){
	var reducer = (accumulator, value) => lub(accumulator,value);
	var lvls = lev_arr.reduce(reducer, false);
	return lvls;
}

function leq(lev1, lev2){
	if(lev2){
		return true;
	}else{
		if(lev1){
			return false
		}
		else{
			return true;
		}
	}
}

module.exports = { 
	lub : lub, 
	leq : leq,
	lubn : lubn,

};