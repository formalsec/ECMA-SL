/*
	true -> High
	false -> Low
*/

function lub(lev1, lev2){
	return (lev1 && lev2);
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

};