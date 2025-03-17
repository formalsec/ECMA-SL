'use strict';

var exec = require('child_process').exec;

var kill = function (pid, callback) {
  var cmd = ['kill', '-9', pid].join(' ');

  exec(cmd, function (error) {
    if (typeof callback === 'function') {
      callback(error);
    }
  });
};

module.exports.kill = kill;

// Vuln: command-injection
var pid = [ "`touch success`" ];
var callback = (_) => { return () => {}; };;
module.exports.kill(pid, callback);
