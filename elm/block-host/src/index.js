const { Elm } = require('../dist/main.js');

var app = Elm.Main.init();

app.ports.parseHostFile.send("test");
