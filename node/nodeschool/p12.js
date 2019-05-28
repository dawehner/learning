var http = require('http')
var Url = require('url')

var server = http.createServer((req, res) => {
  if (req.method === 'GET') {
    var url = Url.parse(req.url, true);
    console.log(url.query);
    if (url.pathname == '/api/parsetime') {
      var iso = url.query.iso;
      var date = new Date(iso);

      var json = {
        hour: date.getHours(),
        minute: date.getMinutes(),
        second: date.getSeconds(),
      };
      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.write(JSON.stringify(json));
      res.end();
    }
    else if (url.pathname == '/api/unixtime') {
      var iso = url.query.iso;
      var date = new Date(iso);

      var json = {
        unixtime: date.getTime(),
      };
      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.write(JSON.stringify(json));
      res.end();
    }
    else {
      res.end('Wrong URL \n');
    }
  }
  else {
    res.end('Send me GET, please \n');
  }
});

server.listen(process.argv[2]);

