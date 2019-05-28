var http = require('http')
var map = require('through2-map')

var server = http.createServer((req, res) => {
  if (req.method === 'POST') {
    req.pipe(map((chunk) => {
      return chunk.toString().toUpperCase();
    })).pipe(res);
  }
  else {
    res.end('Send me POST, please \n');
  }
});

server.listen(process.argv[2]);

