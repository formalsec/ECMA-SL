const Cla = function Cla() {
};
const ev = function ev(x) {
    const v1 = eval(x);
    return v1;
};
Cla.ev = ev;
module.exports = new Cla();