class Heap{

	constructor(){
		this.obj_counter = 0;
		this.heap =[];
	}

	createObject(){
		var obj_name = "_obj_"+ this.obj_counter;
		this.heap[obj_name]=[];
		this.obj_counter++;
		return obj_name;
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

}

module.exports = Heap;
