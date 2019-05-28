var http = require('http');
var async = require('async');
var fetch = require('node-fetch');

var res = [];
var proms = process.argv.slice(2).map((url) => {
  return fetch(url)
  .then((res) => { return res.text(); })
});

Promise.all(proms)
  .then((results) => {
    results.forEach((str) => { console.log(str); });
  })
  .catch(console.error);
