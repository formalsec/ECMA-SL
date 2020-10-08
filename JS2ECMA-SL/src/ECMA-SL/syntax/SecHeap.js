const Heap = require('./Heap');

class SecHeap extends Heap{


	createObject(location, pc_lvl){
		this.heap[location] = {sec_object: [], struct_lvl : pc_lvl, obj_lvl : pc_lvl};
	}

	fieldCheck(object,field){
		var obj = this.heap[object].sec_object;
		if(obj == undefined) return false;
			else {
				if (obj[field]== undefined) return false;
				else return true;
			}
	}
	locationCheck(object){
		var obj = this.heap[object];
		if(obj == undefined) return false;
		else return true;
	}
	getField(object,field){
		return this.heap[object][field];
	}
	getObject(object){
		return this.heap[object];
	}
	deleteObject(object){
		delete this.heap[object];
	}
	deleteField(object, field){
		delete this.heap[object][field];
	}
	setSecObj(object,field, exists_lvl, val_lvl){
		this.heap[object].sec_object[field] = {};
		this.heap[object].sec_object[field].exists_lvl = exists_lvl;
		this.heap[object].sec_object[field].val_lvl = val_lvl;
	}
	setStructLvl(location, lvl){
		this.heap[location].struct_lvl = lvl;
	}
	setObjectLvl(location, lvl){
		this.heap[location].objects_lvl = lvl;
	}
	setFieldLvls(location, field, lvl){
		this.heap[location].sec_object[field] = {};
		this.heap[location].sec_object[field].exists_lvl = lvl;
		this.heap[location].sec_object[field].val_lvl = lvl;
	}
	setFieldValLvl(location, field, lvl){
		this.heap[location].sec_object[field].val_lvl = lvl;
	}
	setFieldExistsLvl(location, field, lvl){
		this.heap[location].sec_object[field].exists_lvl = lvl;
	}
	getStructLvl(location){
		return this.heap[location].struct_lvl;
	}
	getObjectLvl(location){
		return this.heap[location].object_lvl;
	}
	getFieldValLvl(location,field){
		return this.heap[location].sec_object[field].val_lvl;
	}
	getFieldExistsLvl(location, field){
		return this.heap[location].sec_object[field].exists_lvl;
	}



}

module.exports = SecHeap;
