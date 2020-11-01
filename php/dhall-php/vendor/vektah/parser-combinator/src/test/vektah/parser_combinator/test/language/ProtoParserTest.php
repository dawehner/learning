<?php

namespace vektah\parser_combinator\test\language;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\language\ProtoParser;
use vektah\parser_combinator\language\proto\Enum;
use vektah\parser_combinator\language\proto\EnumValue;
use vektah\parser_combinator\language\proto\Extend;
use vektah\parser_combinator\language\proto\Extensions;
use vektah\parser_combinator\language\proto\Field;
use vektah\parser_combinator\language\proto\Identifier;
use vektah\parser_combinator\language\proto\Import;
use vektah\parser_combinator\language\proto\Message;
use vektah\parser_combinator\language\proto\Option;
use vektah\parser_combinator\language\proto\Package;
use vektah\parser_combinator\language\proto\Rpc;
use vektah\parser_combinator\language\proto\Service;

class ProtoParserTest extends TestCase
{
    /**
     * @var ProtoParser
     */
    private $parser;

    public function setUp()
    {
        $this->parser = new ProtoParser();
    }

    public function testBasicMessage()
    {
        $this->assertEquals([
            new Message('Foo', [
                new Field('required', 'int32', 'foo', 1)
            ])
        ], $this->parser->parse('message Foo {required int32 foo = 1;}')->elements);
    }

    public function testIntTypes()
    {
        $input = '
            message Foo {
                required int32 foo = 1;
                required int32 bar = 0x2;
                required int32 baz = 04;
                required int32 fit = 0xFF;
                required int32 far = 077;
            }
        ';

        $expected = [
            new Message('Foo', [
                new Field('required', 'int32', 'foo', 1),
                new Field('required', 'int32', 'bar', 0x2),
                new Field('required', 'int32', 'baz', 04),
                new Field('required', 'int32', 'fit', 0xFF),
                new Field('required', 'int32', 'far', 077),
            ])
        ];

        $this->assertEquals($expected, $this->parser->parse($input)->elements);
    }

    public function testImport()
    {
        $this->assertEquals([new Import('asdf')], $this->parser->parse('import "asdf";')->elements);
    }

    public function testComment()
    {
        $this->assertEquals([new Import('asdf'), new Import('hjkl')], $this->parser->parse('
            import "asdf";
            // Ignore meeee
            import "hjkl";
        ')->elements);
    }

    public function testOption()
    {
        $this->assertEquals([new Option('hello_world', true)], $this->parser->parse('option (hello_world) = true;')->elements);
        $this->assertEquals([new Option('hello_world', 'OK')], $this->parser->parse('option hello_world = "OK";')->elements);
        $this->assertEquals([new Option('.hello_world', 123)], $this->parser->parse('option .hello_world = 123;')->elements);
    }

    public function testEnum()
    {
        $this->assertEquals([new Enum('Foo', [
            new EnumValue('asdf', 0),
            new EnumValue('qwer', 1),
        ])], $this->parser->parse('enum Foo {
            asdf = 0;
            qwer = 1;
        }')->elements);

        $this->assertEquals([new Enum('Foo', [
            new EnumValue('asdf', 10),
            new EnumValue('qwer', 20),
        ])], $this->parser->parse('enum Foo {
            asdf = 10;
            qwer = 20;
        }')->elements);
    }

    public function testCombination()
    {
        $input = '
            message Bar {
                required MessageType type = 0;
                required bytes data = 1;

                enum Foo {
                    asdf = 0;
                    qwer = 1;
                }
            }
        ';

        $expected = [
            new Message('Bar', [
                new Field('required', 'MessageType', 'type', 0),
                new Field('required', 'bytes', 'data', 1),
                new Enum('Foo', [
                    new EnumValue('asdf', 0),
                    new EnumValue('qwer', 1)
                ])
            ])
        ];

        $this->assertEquals($expected, $this->parser->parse($input)->elements);
    }

    public function testExtensions()
    {
        $this->assertEquals([new Message('Foo', [new Extensions(10, new Identifier('max'))])], $this->parser->parse('
            message Foo {
                extensions 10 to max;
            }
        ')->elements);
    }

    public function testExtend()
    {
        $this->assertEquals([new Extend('Foo', [new Field('optional', 'int32', 'foo', 10)])], $this->parser->parse('
            extend Foo {
                optional int32 foo = 10;
            }
        ')->elements);
    }

    public function testPackage()
    {
        $this->assertEquals([new Package('foobar')], $this->parser->parse('
            package foobar;
        ')->elements);
    }

    public function testService()
    {
        $this->assertEquals([new Service('Foo', [
            new Rpc('Test', 'TestRequest', 'TestResponse')
        ])], $this->parser->parse('
            service Foo {
                rpc Test (TestRequest) returns (TestResponse);
            }
        ')->elements);
    }

    public function testDefaultFeildOption()
    {
        $this->assertEquals([new Message('Foo', [
            new Field('required', 'int32', 'bar', 123, [
                new Option('default', 555),
            ])
        ])], $this->parser->parse('
            message Foo {
                required int32 bar = 123 [default = 555];
            }
        ')->elements);
    }

    public function testFieldOption()
    {
        $this->assertEquals([new Message('Foo', [
            new Field('required', 'int32', 'bar', 123, [
                new Option('asdf', 555),
                new Option('hjkl', 666)
            ])
        ])], $this->parser->parse('
            message Foo {
                required int32 bar = 123 [(asdf) = 555, (hjkl) = 666];
            }
        ')->elements);
    }

    public function testEmptyMessage()
    {
        $this->assertEquals([new Message('Foo', [])], $this->parser->parse('message Foo{}')->elements);
    }

    public function testRecurse()
    {
        $out = $this->parser->parse('
            message Bar {
                required MessageType type = 0;
                required bytes data = 1;

                enum Foo {
                    asdf = 0;
                    qwer = 1;
                }
            }
        ');

        $flattened = [];

        $out->traverse(function ($element, $namespace) use (&$flattened) {
            $flattened[] = $namespace;
            $flattened[] = $element;
        });

        $this->assertEquals([
            [],
            new Message('Bar', [
                new Field('required', 'MessageType', 'type', 0),
                new Field('required', 'bytes', 'data', 1),

                new Enum('Foo', [
                    new EnumValue('asdf', 0),
                    new EnumValue('qwer', 1),
                ])
            ]),
            ['Bar'],
            new Field('required', 'MessageType', 'type', 0),
            ['Bar'],
            new Field('required', 'bytes', 'data', 1),
            ['Bar'],
            new Enum('Foo', [
                new EnumValue('asdf', 0),
                new EnumValue('qwer', 1),
            ]),
            ['Bar', 'Foo'],
            new EnumValue('asdf', 0),
            ['Bar', 'Foo'],
            new EnumValue('qwer', 1),
        ], $flattened);
    }
}
