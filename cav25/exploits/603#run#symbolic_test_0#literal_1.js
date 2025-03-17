var exec = require('child_process').exec
module.exports = function(app, cb) {
  exec('heroku config --app ' + app, function(err, stdout) {
    if(err) return cb(err);
    var config = {}
    var lines = stdout.split('\n')
    lines.shift()
    lines.forEach(function(line) {
      if(!line.trim()) return;
      var parts = line.split(': ')
      if(!parts[1]) {
        console.log('could not parse', line)
      } else {
        config[parts[0].trim()] = parts[1].trim()
      }
    })
    cb(null, config)
  })
}

// Vuln: command-injection
var app = "`touch success`";
var cb = (_) => { return () => {}; };;
module.exports(app, cb);
