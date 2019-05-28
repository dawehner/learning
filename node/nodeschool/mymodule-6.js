var fs = require('fs');
var path = require('path');

module.exports = (dirname, filetype, callback) => {
  fs.readdir(dirname, (err, list) => {
    if (err) { return callback(err); }
    var filteredList = list.filter((elem) => {
      var ext = path.extname(elem);
      return ext == "." + filetype;
    });

    callback(null, filteredList);
  });
}
