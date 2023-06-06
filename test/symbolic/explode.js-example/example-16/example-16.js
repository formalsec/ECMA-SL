// testing taint from parameter to eval function call
// source is a class method

class Cla { 
    ev(x) {
       return eval(x); 
    }
}

module.exports = new Cla;