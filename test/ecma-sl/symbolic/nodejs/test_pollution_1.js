/* Ok */
Object.sealProperties(Object.prototype);
let obj = {};
obj.toString = 'polluted';
