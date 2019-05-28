var http = require('http');
var fs = require('fs');

var server = http.createServer((req, res) => {
  var fileStream = fs.createReadStream(process.argv[3]);
  fileStream.pipe(res);
});
server.listen(process.argv[2]);
