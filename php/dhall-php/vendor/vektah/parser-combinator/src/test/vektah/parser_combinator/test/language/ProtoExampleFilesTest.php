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

class ProtoExampleFilesTest extends TestCase
{
    private $parser;

    public function __construct()
    {
        $this->parser = new ProtoParser();
    }

    public function testSample1()
    {
        // From https://metacpan.org/module/Google::ProtocolBuffers
        $input = '
             message Person {
              required string name  = 1;
              required int32 id     = 2; // Unique ID number for this person.
              optional string email = 3;

              enum PhoneType {
                MOBILE = 0;
                HOME = 1;
                WORK = 2;
              }

              message PhoneNumber {
                required string number = 1;
                optional PhoneType type = 2 [default = HOME];
              }

              repeated PhoneNumber phone = 4;
            }
        ';

        $expected = [
            new Message('Person', [
                new Field('required', 'string', 'name', 1),
                new Field('required', 'int32', 'id', 2),
                new Field('optional', 'string', 'email', 3),

                new Enum('PhoneType', [
                    new EnumValue('MOBILE', 0),
                    new EnumValue('HOME', 1),
                    new EnumValue('WORK', 2)
                ]),

                new Message('PhoneNumber', [
                    new Field('required', 'string', 'number', 1),
                    new Field('optional', 'PhoneType', 'type', 2, [new Option('default', new Identifier('HOME'))]),
                ]),

                new Field('repeated', 'PhoneNumber', 'phone', 4),
            ])
        ];

        $this->assertEquals($expected, $this->parser->parse($input)->elements);
    }

    public function testSample2()
    {
        // From https://metacpan.org/module/Google::ProtocolBuffers
        $input = '
            package some_package;
            // message Plugh contains one regular field and three extensions
            message Plugh {
                optional int32 foo = 1;
                extensions 10 to max;
            }
            extend Plugh {
                optional int32 bar = 10;
            }
            message Thud {
                extend Plugh {
                    optional int32 baz = 11;
                }
            }

            // Note: the official Google\'s proto compiler does not allow
            // several package declarations in a file (as of version 2.0.1).
            // To compile this example with the official protoc, put lines
            // above to some other file, and import that file here.
            package another_package;
            // import \'other_file.proto\';

            extend some_package.Plugh {
            optional int32 qux = 12;
            }
        ';

        $expected = [
            new Package('some_package', [
                new Message('Plugh', [
                    new Field('optional', 'int32', 'foo', 1),
                    new Extensions(10, new Identifier('max')),
                ]),

                new Extend('Plugh', [
                    new Field('optional', 'int32', 'bar', 10)
                ]),

                new Message('Thud', [
                    new Extend('Plugh', [
                        new Field('optional', 'int32', 'baz', 11)
                    ])
                ]),
            ]),

            new Package('another_package', [
                new Extend('some_package.Plugh', [
                    new Field('optional', 'int32', 'qux', 12)
                ])
            ]),
        ];

        $this->assertEquals($expected, $this->parser->parse($input)->elements);
    }

    public function testSample3()
    {
        // From https://developers.google.com/protocol-buffers/docs/proto
        $input = '
            import "google/protobuf/descriptor.proto";

            extend google.protobuf.FileOptions {
              optional string my_file_option = 50000;
            }
            extend google.protobuf.MessageOptions {
              optional int32 my_message_option = 50001;
            }
            extend google.protobuf.FieldOptions {
              optional float my_field_option = 50002;
            }
            extend google.protobuf.EnumOptions {
              optional bool my_enum_option = 50003;
            }
            extend google.protobuf.EnumValueOptions {
              optional uint32 my_enum_value_option = 50004;
            }
            extend google.protobuf.ServiceOptions {
              optional MyEnum my_service_option = 50005;
            }
            extend google.protobuf.MethodOptions {
              optional MyMessage my_method_option = 50006;
            }

            option (my_file_option) = "Hello world!";

            message MyMessage {
              option (my_message_option) = 1234;

              optional int32 foo = 1 [(my_field_option) = 4.5];
              optional string bar = 2;
            }

            enum MyEnum {
              option (my_enum_option) = true;

              FOO = 1 [(my_enum_value_option) = 321];
              BAR = 2;
            }

            message RequestType {}
            message ResponseType {}

            service MyService {
              option (my_service_option) = FOO;

              rpc MyMethod(RequestType) returns(ResponseType) {
                // Note:  my_method_option has type MyMessage.  We can set each field
                //   within it using a separate "option" line.
                option (my_method_option).foo = 567;
                option (my_method_option).bar = "Some string";
              }
            }
        ';

        $expected = [
            new Import('google/protobuf/descriptor.proto'),

            new Extend('google.protobuf.FileOptions', [
                new Field('optional', 'string', 'my_file_option', 50000)
            ]),

            new Extend('google.protobuf.MessageOptions', [
                new Field('optional', 'int32', 'my_message_option', 50001)
            ]),

            new Extend('google.protobuf.FieldOptions', [
                new Field('optional', 'float', 'my_field_option', 50002)
            ]),

            new Extend('google.protobuf.EnumOptions', [
                new Field('optional', 'bool', 'my_enum_option', 50003)
            ]),

            new Extend('google.protobuf.EnumValueOptions', [
                new Field('optional', 'uint32', 'my_enum_value_option', 50004)
            ]),

            new Extend('google.protobuf.ServiceOptions', [
                new Field('optional', 'MyEnum', 'my_service_option', 50005)
            ]),

            new Extend('google.protobuf.MethodOptions', [
                new Field('optional', 'MyMessage', 'my_method_option', 50006)
            ]),

            new Option('my_file_option', 'Hello world!'),

            new Message('MyMessage', [
                new Option('my_message_option', 1234),

                new Field('optional', 'int32', 'foo', 1, [
                    new Option('my_field_option', 4.5)
                ]),
                new Field('optional', 'string', 'bar', 2),
            ]),

            new Enum('MyEnum', [
                new EnumValue('FOO', 1, [
                    new Option('my_enum_value_option', 321)
                ]),
                new EnumValue('BAR', 2),
            ], [
                new Option('my_enum_option', true),
            ]),

            new Message('RequestType', []),

            new Message('ResponseType', []),

            new Service('MyService', [
                new Option('my_service_option', new Identifier('FOO')),

                new Rpc('MyMethod', 'RequestType', 'ResponseType', [
                    new Option('my_method_option.foo', 567),
                    new Option('my_method_option.bar', "Some string"),
                ])
            ]),
        ];

        $this->assertEquals($expected, $this->parser->parse($input)->elements);
    }
}
