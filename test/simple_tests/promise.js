var count = 0;

var p = new Promise(function (resolve, reject) {
    count++;
    resolve(2)
})
p.then(function (v) {
    count += v;
    console.log("2 - " + count)
    return 3;
}).then(function (v) {
    count += v;
    console.log("3 - " + count)
});

// 
console.log("1 - " + count)