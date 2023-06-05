// testing taint from parameter to eval function call
// testing arguments source

module.exports = function f() {
    arg0 = arguments[0];
    arg1 = arguments[1];
    len = arguments.length;

    var x = arg1;
    for(let i = 0; i < len; i++) {
        if (x.prop == arguments[i].prop){
            return eval(arguments[i].prop);
        }
    }
}