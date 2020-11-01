<?php

namespace vektah\parser_combinator\language;


use vektah\parser_combinator\Input;
use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\exception\ParseException;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Ignore;
use vektah\parser_combinator\parser\EofParser;
use vektah\parser_combinator\parser\WhitespaceParser;
use vektah\parser_combinator\parser\literal\FloatLiteral;
use vektah\parser_combinator\parser\literal\IntLiteral;
use vektah\parser_combinator\parser\literal\StringLiteral;

class JsonParser
{
    private $rootParser;

    public function __construct()
    {
        $ws = new WhitespaceParser();
        $left_brace = new Ignore('{');
        $right_brace = new Ignore('}');
        $left_bracket = new Ignore('[');
        $right_bracket = new Ignore(']');
        $colon = new Ignore(':');
        $comma = new Ignore(',');

        // Types
        $bool = new Closure(new Choice(['true', 'false']), function($data) {
            return $data === 'true';
        });

        $null = new Closure('null', function($data) {
            return null;
        });

        $string = new StringLiteral();
        $int = new IntLiteral();
        $float = new FloatLiteral();

        $value = new Choice($bool, $null, $string, $float, $int);

        $object = new Closure(new Sequence(
            [
                $ws,
                $left_brace,
                $ws,
                new Many(
                    [
                        new Sequence(
                            [
                                $string,
                                $ws,
                                $colon,
                                $ws,
                                $value,
                                $ws
                            ]
                        )
                    ],
                    0,
                    1
                ),
                new Many(
                    [
                        new Sequence(
                            [
                                $comma,
                                $ws,
                                $string,
                                $ws,
                                $colon,
                                $ws,
                                $value
                            ]
                        ),
                    ]
                ),
                $ws,
                $right_brace,
                $ws
            ]
        ), function ($data) {
            $result = [];

            foreach ($data as $value) {
                foreach ($value as $value1) {
                    $result[$value1[0]] = $value1[1];
                }
            }

            return $result;
        });

        $value->append($object);
        $array = new Closure(new Sequence(
            $ws,
            $left_bracket,
            $ws,
            // First one has no comma
            new Many([$value], 0, 1),
            new Many([new Sequence([$ws, $comma, $ws, $value])]),
            $ws,
            $right_bracket,
            $ws
        ), function($data) {
            $result = [];

            if (is_array($data[0])) {
                $result[] = $data[0][0];
            }

            if (is_array($data[1])) {
                foreach ($data[1] as $value) {
                    $result[] = $value[0];
                }
            }

            return $result;
        });
        $value->append($array);

        $this->rootParser = new Closure(new Sequence($value, new EofParser()), function($data) {
            return $data[0];
        });
    }

    public function parse(Input $input)
    {
        $result = $this->rootParser->parse($input);

        if ($result->errorMessage) {
            throw new ParseException($result->errorMessage);
        } else {
            return $result->data;
        }
    }
}
