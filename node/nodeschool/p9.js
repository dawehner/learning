var net = require('net');

var server = net.createServer((socket) => {
  var date = new Date();
  var year = date.getFullYear();
  var month = date.getMonth();
  var dayMonth = date.getDate();
  var hours = date.getHours();
  var minutes = date.getMinutes();

  socket.write(year + "-" + (month + 1) + "-" + dayMonth + " " + hours + ":" + minutes + "\n");
  socket.end();
});
server.listen(process.argv[2]);
