var fs = require('fs');

var filename = process.argv[2];

fs.readFile(filename, (err, data) => {
  var str = data.toString();

  console.log(str.split("\n").length - 1);
});


