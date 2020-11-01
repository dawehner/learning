parser-combinator [![Build Status](https://travis-ci.org/Vektah/parser-combinator.png)](https://travis-ci.org/Vektah/parser-combinator)
=================

A simple parser combinator framework written in PHP.

Google protobuffer parser:
--------------------------
```
$parser = new ProtoParser();
$out = $parser->parse('
    message Bar {
        required MessageType type = 0;
        required bytes data = 1;

        enum Foo {
            asdf = 0;
            qwer = 1;
        }
    }
');

// $out will contain return a parse tree that looks like this:
[
    new Message('Bar', [
        new Field('required', 'MessageType', 'type', 0),
        new Field('required', 'bytes', 'data', 1),
        new Enum('Foo', [
            new EnumValue('asdf', 0),
            new EnumValue('qwer', 1)
        ])
    ])
];

```

Json parser:
------------
The only reason you would use this parser is for error messages. It is much slower then the C library used by php to parse json. It could be used to automatically reparse json in failure cases and display helpful error messages.
```
$parser = new JsonParser();
$out = $parser->parse('{"asdf": { "foo" : "bar" }}');

// $out will contain a parse tree that looks like this:
[
  'asdf' => [
    'foo' => 'bar'
  ]
]
```
