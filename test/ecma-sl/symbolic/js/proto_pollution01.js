Object.sealProperties(Object.prototype);

let obj = {};
/* Ok */
obj.toString = 'polluted';
