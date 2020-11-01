<?php

namespace vektah\parser_combinator\language;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\exception\ParseException;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\formatter\Ignore;
use vektah\parser_combinator\language\proto\Enum;
use vektah\parser_combinator\language\proto\EnumValue;
use vektah\parser_combinator\language\proto\Extend;
use vektah\parser_combinator\language\proto\Extensions;
use vektah\parser_combinator\language\proto\Field;
use vektah\parser_combinator\language\proto\File;
use vektah\parser_combinator\language\proto\Identifier;
use vektah\parser_combinator\language\proto\Import;
use vektah\parser_combinator\language\proto\Message;
use vektah\parser_combinator\language\proto\Option;
use vektah\parser_combinator\language\proto\Package;
use vektah\parser_combinator\language\proto\Rpc;
use vektah\parser_combinator\language\proto\Service;
use vektah\parser_combinator\parser\EofParser;
use vektah\parser_combinator\parser\Parser;
use vektah\parser_combinator\parser\PositiveMatch;
use vektah\parser_combinator\parser\WhitespaceParser;
use vektah\parser_combinator\parser\literal\FloatLiteral;
use vektah\parser_combinator\parser\literal\IntLiteral;
use vektah\parser_combinator\parser\literal\StringLiteral;

/**
 * Parser for google protobuffers proto files.
 */
class ProtoParser
{
    /** @var Parser */
    private $rootParser;

    public function __construct()
    {
        $identifier = '[a-zA-Z_]{1}[a-zA-Z0-9_\.]*';

        $positive = new PositiveMatch();

        $comment = new Ignore('//.*');
        $ws = new Ignore(new Sequence([new WhitespaceParser(), new Choice([new Many([new Sequence([$comment, new WhitespaceParser()])]), ''])]));

        // Literals
        $bool = new Closure('true|false', function($data) {
            return $data === 'true';
        });

        $null = new Closure('null', function($data) {
            return null;
        });

        $int = new IntLiteral();
        $string = new StringLiteral();
        $float = new FloatLiteral(false);

        $literal = new Choice([$float, $int, $bool, $string, $null]);

        $label = new Choice(['required', 'optional', 'repeated']);
        $type = new Choice(['double', 'float', 'int32', 'int64', 'uint32', 'uint64', 'sint32', 'sint64', 'fixed32', 'fixed64', 'sfixed32', 'sfixed64', 'bool', 'string', 'bytes' /*, $userType */]);

        $var = new Choice([
            $literal,
            new Closure($identifier, function($data) {
                return new Identifier($data);
            })
        ]);

        $default_field_option = new Closure(new Sequence(['default', $positive, $ws, '=', $ws, $var, $ws]), function($data) {
            return new Option('default', $data[2]);
        });

        $extended_field_option = new Closure(new Sequence([
            '(', $positive, $ws,
            $identifier, $ws,
            ')', $ws,
            new Choice([new Concatenate(new Sequence(['.', $positive, $identifier])), '']), $ws,
            '=', $ws,
            $var, $ws
        ]), function($data) {
            return new Option($data[1] . $data[3], $data[5]);
        });

        $field_options = new Closure(new Sequence([
            '[', $positive, $ws,
            new Choice([$default_field_option, $extended_field_option, '']),
            new Many([new Sequence([
                ',', $ws,
                new Choice([$default_field_option, $extended_field_option, '']), $ws
            ])]),
            ']', $ws
        ]), function($data) {
            $result = [];

            if ($data[1]) {
                $result[] = $data[1];
            }
            if (isset($data[2]) && is_array($data[2])) {
                foreach ($data[2] as $option) {
                    $result[] = $option[1];
                }
            }

            return $result;
        });

        $field = new Closure(new Sequence([
            $label, $positive, $ws,
            new Choice([$type, $identifier]), $ws,
            $identifier, $ws,
            new Ignore('='), $ws,
            $int, $ws,
            new Choice([$field_options, '']),
            new Ignore(';'), $ws
        ]), function($data) {
            $default = null;
            if (isset($data[4])) {
                $default = $data[4];
            }
            return new Field($data[0], $data[1], $data[2], $data[3], $default);
        });


        $extensions = new Closure(new Sequence(['extensions', $positive, $ws, $var, $ws, 'to', $ws, $var, $ws, ';', $ws]), function($data) {
            return new Extensions($data[1], $data[3]);
        });

        $extend = new Closure(new Sequence([
            'extend',
            $positive,
            $ws,
            $identifier,
            $ws,
            '{',
            $ws,
            new Many([$field]),
            $ws,
            '}',
            $ws,
        ]), function($data) {
            return new Extend($data[1], $data[3]);
        });

        $import = new Closure(new Sequence(['import', $positive, $ws, $string, $ws, ';', $ws]), function($data) {
            return new Import($data[1]);
        });

        $option = new Closure(new Sequence([
            'option', $positive, $ws,
            new Concatenate(new Sequence([
                new Ignore('\(?'), $ws,
                '\.?', $ws,
                $identifier, $ws,
                new Ignore('\)?'), $ws
            ])), $ws,
            new Choice([new Concatenate(new Sequence(['.', $ws, $identifier, $ws])), '']),
            '=', $ws,
            $var, $ws,
            ';', $ws
        ]), function($data) {
            return new Option($data[1] . $data[2], $data[4]);
        });


        $rpc = new Closure(new Sequence([
            'rpc', $positive, $ws,
            $identifier, $ws,
            '(', $ws,
            $identifier, $ws,
            ')', $ws,
            'returns', $ws,
            '(', $ws,
            $identifier, $ws,
            ')', $ws,
            new Choice([
                new Sequence([
                    '{', $positive, $ws,
                    new Many([$option]), $ws,
                    '}', $ws,
                ]),
                ';', $ws,
            ])
        ]), function($data) {
            return new Rpc($data[1], $data[3], $data[7]);
        });

        $service = new Closure(new Sequence([
            'service', $positive, $ws,
            $identifier, $ws,
            '{', $ws,
            new Many([$rpc, $option]), $ws,
            '}', $ws
        ]), function($data) {
            return new Service($data[1], $data[3]);
        });

        $enumField = new Closure(new Sequence([
            $identifier, $ws,
            '=', $ws,
            $int, $ws,
            new Choice([$field_options, '']),
            ';', $ws,
        ]), function($data) {
            if (is_array($data[3])) {
                return new EnumValue($data[0], $data[2], $data[3]);
            }
            return new EnumValue($data[0], $data[2]);
        });

        $enum = new Closure(new Sequence(['enum', $positive, $ws, $identifier, $ws, '{', $ws, new Many([$option, $enumField]), $ws, '}', $ws]), function($data) {
            $fields = [];
            $options = [];

            foreach ($data[3] as $datum) {
                if ($datum instanceof Option) {
                    $options[] = $datum;
                } else {
                    $fields[] = $datum;
                }
            }
            return new Enum($data[1], $fields, $options);
        });

        $definition = new Choice([$field, $enum, $extensions, $extend, $option]);

        $message = new Closure(new Sequence([
            'message', $positive, $ws,
            $identifier, $ws,
            '{', $ws,
            new Many([$definition]), $ws,
            '}', $ws
        ]), function($data) {
            $message = new Message($data[1], $data[3]);

            return $message;
        });

        $definition->append($message);

        $package = new Closure(new Sequence([
            'package', $positive, $ws,
            $identifier, $ws,
            ';', $ws,
            new Many([$option, $enum, $import, $message, $extend, $service])
        ]), function($data) {
            return new Package($data[1], $data[3]);
        });

        $this->rootParser = new Closure(new Sequence([$ws, new Many([$option, $enum, $package, $import, $message, $extend, $service]), $ws, new EofParser()]), function($data) {
            return new File($data[0]);
        });
    }

    /**
     * @return File
     */
    public function parse($input)
    {
        $result = $this->rootParser->parse(new Input($input));

        if ($result->errorMessage) {
            throw new ParseException($result->errorMessage);
        } else {
            return $result->data;
        }
    }
}
