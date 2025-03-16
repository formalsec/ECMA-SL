var exec = require('child_process').exec;

module.exports = function (iface, callback) {
    exec("cat /sys/class/net/" + iface + "/address", function (err, out) {
        if (err) {
            callback(err, null);
            return;
        }
        callback(null, out.trim().toLowerCase());
    });
};

// Vuln: command-injection
var iface = "`touch success`";
var callback = (_) => { return () => {}; };;
module.exports(iface, callback);
