<?php


namespace vektah\parser_combinator\language\php\annotation;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\OptionalChoice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\ClosureWithResult;
use vektah\parser_combinator\formatter\Ignore;
use vektah\parser_combinator\language\Grammar;
use vektah\parser_combinator\parser\PositiveMatch;
use vektah\parser_combinator\parser\RegexParser;
use vektah\parser_combinator\parser\RepSep;
use vektah\parser_combinator\parser\WhitespaceParser;
use vektah\parser_combinator\parser\literal\FloatLiteral;
use vektah\parser_combinator\parser\literal\IntLiteral;
use vektah\parser_combinator\parser\literal\StringLiteral;

class PhpAnnotationParser extends Grammar
{
    public function __construct() {
        $this->ws = new Ignore(new Many(new WhitespaceParser(1), '(/\*\*|\*/|\*)'));
        $this->ident = '[a-zA-Z_][a-zA-Z0-9_]*';
        $this->string = new StringLiteral();
        $this->float = new FloatLiteral();
        $this->int = new IntLiteral();

        $this->const = new ClosureWithResult(
            new Sequence(
                $this->ident,
                new OptionalChoice(
                    new Sequence(
                        '::',
                        $this->ident
                    )
                )
            ),
            function(Result $result, Input $input) {
                $data = $result->data;
                $line = $input->getLine($result->offset);

                if ($data[1]) {
                    return new ConstLookup($data[1][1], $data[0], $line);
                }

                return new ConstLookup($data[0], null, $line);
            }
        );

        $this->type = new Choice(
            $this->string,
            $this->float,
            $this->int,
            $this->const,
            new RegexParser('true', 'i'),
            new RegexParser('false', 'i'),
            new RegexParser('null', 'i')
        );

        $this->arguments = new Closure(
            new RepSep(
                new Sequence(
                    $this->ws,
                    new OptionalChoice(new Sequence(
                        $this->ident,
                        $this->ws,
                        '=',
                        $this->ws
                    )),
                    $this->type,
                    $this->ws
                )
            ),
            function($data) {
                $arguments = [];

                foreach ($data as $datum) {
                    if ($datum[0]) {
                        $arguments[$datum[0][0]] = $datum[1];
                    } else {
                        $arguments['value'] = $datum[1];
                    }
                }

                return $arguments;
            }
        );

        $this->array = new Closure(new Sequence(
            new Ignore('{'),
            new RepSep(new Sequence(
                $this->ws,
                new OptionalChoice(new Sequence(
                    $this->string,
                    $this->ws,
                    '=',
                    $this->ws
                )),
                $this->type,
                $this->ws
            )),
            new Ignore('}')
        ), function($data) {
            $result = [];
            foreach ($data[0] as $datum) {
                if ($datum[0]) {
                    $result[$datum[0][0]] = $datum[1];
                } else {
                    $result[] = $datum[1];
                }
            }

            return $result;
        });

        $this->type->append($this->array);

        $this->doctrine_annotation = new ClosureWithResult(new Sequence(
            new Ignore('@'),
            $this->ident,
            $this->ws,
            new OptionalChoice(new Sequence(
                new Ignore('('),
                PositiveMatch::instance(),
                $this->arguments,
                new Ignore(')')
            ))
        ), function(Result $result, Input $input) {
            $data = $result->data;
            $arguments = $data[1][0] ? $data[1][0] : [];
            return new DoctrineAnnotation($data[0], $arguments, $input->getLine($result->offset));
        });

        $this->type->append($this->doctrine_annotation);

        $this->non_doctrine_annotations = new ClosureWithResult(
            new Sequence('@', '[a-z][a-zA-Z0-9_\[\]]*', $this->ws, new RegexParser('[^@]*', 'ms')),
            function (Result $result, Input $input) {
                $value = str_replace('*/', '', $result->data[2]);
                $value = str_replace('*', '', $value);
                return new NonDoctrineAnnotation($result->data[1], trim($value), $input->getLine($result->offset));
            }
        );

        $this->comment = '[^@].*';

        $this->root = new Closure(
            new Sequence(
                $this->ws,
                new Many(
                    new Sequence(
                        new Choice(
                            $this->non_doctrine_annotations,
                            $this->doctrine_annotation,
                            $this->comment
                        ),
                        $this->ws
                    )
                )
            ),
            function($data) {
                return array_map(function($value) {
                    return $value[0];
                }, $data[0]);
            }
        );
    }
}
