var assert = require('chai').assert;
var TodoParser = require('../todo-parser');

describe('todo-parser', function() {
  describe('just-text', function() {
    if('parse simple string', function() {
      var todo = TodoParser.parse('hello');
      assert.isOk(todo);
      assert.equal('hello', todo.txt);
    });
  });
});

