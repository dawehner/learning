var fs = require('fs');
var path = require('path');

var dir = process.argv[2];
var filetype = process.argv[3];

fs.readdir(dir, (err, list) => {
  var filteredList = list.filter((elem) => {
    var ext = path.extname(elem);
    return ext == "." + filetype;
  });

  for (let i = 0; i < filteredList.length; i++) {
    console.log(filteredList[i]);
  }
});
