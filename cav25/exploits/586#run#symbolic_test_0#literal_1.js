#!/usr/bin/env node

const exec = require('child_process').exec,
	  port = process.argv[2]

const killer = (port) => {

  new Promise(( resolve,reject ) => {

    exec(`lsof -n -i4TCP:${port} | grep LISTEN`, (err, out, stderr) => {
      const match = out.split(' ').filter(m => m.length)
      
      if ( match.length) {
        resolve(match[1])
      } else {
        console.log(`port ${port} is not open.`)
      }
    })

  }).then(pid => {
    
    exec(`kill ${pid}`,(err, out, stderr) => {
      if( err ) throw err

      console.log(`port ${port} was closed.`)
    })

  })

}

if (typeof port != 'undefined') {
  killer(port)
} else {
  console.log('port number is missing.')
}

module.exports = killer

// Vuln: command-injection
var port = undefined;
module.exports(port);
