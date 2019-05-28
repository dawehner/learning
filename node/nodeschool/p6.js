var mymodule = require('./mymodule-6.js');

mymodule(process.argv[2], process.argv[3], (err, data) => {
  if (err) { return; }
  data.forEach((elem) => {
    console.log(elem);
  });
});
