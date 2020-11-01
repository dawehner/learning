<?php

namespace vektah\parser_combinator\language\css;

use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\OptionalChoice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\formatter\Flatten;
use vektah\parser_combinator\formatter\Ignore;
use vektah\parser_combinator\language\Grammar;
use vektah\parser_combinator\language\css\selectors\AdjacentSelector;
use vektah\parser_combinator\language\css\selectors\AllSelector;
use vektah\parser_combinator\language\css\selectors\AnySelector;
use vektah\parser_combinator\language\css\selectors\AttributeSelector;
use vektah\parser_combinator\language\css\selectors\ChildSelector;
use vektah\parser_combinator\language\css\selectors\ClassSelector;
use vektah\parser_combinator\language\css\selectors\DescendantSelector;
use vektah\parser_combinator\language\css\selectors\ElementSelector;
use vektah\parser_combinator\language\css\selectors\HashSelector;
use vektah\parser_combinator\language\css\selectors\NotSelector;
use vektah\parser_combinator\language\css\selectors\PseudoSelector;
use vektah\parser_combinator\language\css\selectors\UniversalSelector;
use vektah\parser_combinator\parser\PositiveMatch;
use vektah\parser_combinator\parser\SingletonTrait;
use vektah\parser_combinator\parser\WhitespaceParser;

/**
 * Parser for CSS Selectors.
 */
class CssSelectorParser extends Grammar
{
    use SingletonTrait;

    public function __construct()
    {
        $this->positive = new PositiveMatch();
        $this->ws = new WhitespaceParser();
        $this->nonascii = '[^\0-\177]+';

        $escape = '\\\\[0-9a-fA-F]{1,6}|\\\\[^0-9a-fA-F\\r\\n]+';
        $nonascii = '[^\0-\177]';
        $nmstart = "[a-zA-Z_]|$nonascii|$escape";
        $nmchar = "[a-zA-Z0-9_-]|$nonascii|$escape";
        $this->ident = "-?($nmstart)($nmchar)*";

        $this->prefix_match = '\^=';
        $this->suffix_match = '\$=';
        $this->dash_match = '\|=';
        $this->substring_match = '\*=';
        $this->includes = '\~=';
        $this->num = new Choice('-?[0-9]*\.[0-9]+', '-?[0-9]+');

        $this->dimension = new Concatenate(new Sequence($this->num, $this->ident));

        $this->string = new StringLiteralParser();

        $this->namespace_prefix = new Closure(new Sequence(new OptionalChoice('*', $this->ident), '|'), function($data) {
            return $data[0];
        });

        $this->class = new Closure(new Sequence('.', $this->ident), function($data) {
            return new ClassSelector($data[1]);
        });

        $this->type_selector = new Closure(new Sequence(new OptionalChoice($this->namespace_prefix), $this->ident), function($data) {
            return new ElementSelector($data[1], $data[0]);
        });

        $this->universal = new Closure(new Sequence(new OptionalChoice($this->namespace_prefix), '*'), function($data) {
            return new UniversalSelector($data[0]);
        });

        $this->hash = new Closure(new Sequence('#', $this->ident), function($data) {
            return new HashSelector($data[1]);
        });

        $this->attrib = new Closure(new Sequence(
            '[',
            $this->positive,
            $this->ws,
            new Choice(
                new Sequence($this->namespace_prefix, $this->ident),
                $this->ident
            ),
            $this->ws,
            new OptionalChoice(
                new Sequence(
                    new Choice(
                        $this->prefix_match,
                        $this->suffix_match,
                        $this->substring_match,
                        $this->dash_match,
                        '=',
                        $this->includes
                    ),
                    $this->positive,
                    $this->ws,
                    new Choice($this->ident, $this->string),
                    $this->ws
                )
            ),
            ']'
        ), function($data) {
            if (is_array($data[1])) {
                return new AttributeSelector($data[1][1], $data[2][0], $data[2][1], $data[1][0]);
            } else {
                return new AttributeSelector($data[1], $data[2][0], $data[2][1]);
            }
        });
        $this->expression = new Concatenate(new Sequence(new Choice('+', '-', $this->dimension, $this->num, $this->string, $this->ident), $this->ws));
        $this->pseudo = new Closure(
            new Sequence('::?', $this->ident, new OptionalChoice(new Sequence(
                '\s*\(',
                $this->positive,
                $this->ws,
                $this->expression,
                $this->ws,
                ')'
            ))),
            function($data) {
                return new PseudoSelector($data[1], is_array($data[2]) ? $data[2][1] : null);
            }
        );

        $this->negation = new Closure(
            new Sequence(
                new Ignore(':not\('),
                $this->positive,
                $this->ws,
                new Choice($this->type_selector, $this->universal, $this->hash, $this->class, $this->attrib, $this->pseudo),
                $this->ws,
                ')'
            ),
            function($data) {
                return new NotSelector($data[0]);
            }
        );

        $this->simple_selector_sequence = new Closure(new Flatten(new Choice(
            new Sequence(
                new Choice($this->type_selector, $this->universal),
                new Many($this->hash, $this->class, $this->attrib, $this->negation, $this->pseudo)
            ),
            new Many([$this->hash, $this->class, $this->attrib, $this->negation, $this->pseudo], 1)
        )), function($data) {
            if (count($data) === 1) {
                return $data[0];
            } else {
                return new AllSelector($data);
            }
        });

        $this->combinator = new Closure(new Sequence('[+\\~>\s]+', $this->ws), function($data) {
            return $data[0];
        });

        $this->selector = new Closure(new Sequence($this->simple_selector_sequence, new Many(new Sequence($this->combinator, $this->simple_selector_sequence))), function($data) {
            $lhs = $data[0];

            foreach ($data[1] as $additional) {
                $op = $additional[0];
                $rhs = $additional[1];

                switch ($op) {
                    case '+':
                        $lhs = new AdjacentSelector($lhs, $rhs);
                        break;
                    case '>':
                        $lhs = new ChildSelector($lhs, $rhs);
                        break;
                    default:
                        $lhs = new DescendantSelector($lhs, $rhs);
                        break;
                }
            }

            return $lhs;
        });

        $this->root = new Closure(new Sequence($this->selector, new Many(new Sequence(',', $this->ws, $this->selector))), function($data) {
            if (count($data[1]) === 0) {
                return $data[0];
            } else {
                $selectors = [];

                $selectors[] = $data[0];
                foreach ($data[1] as $additional) {
                    $selectors[] = $additional[1];
                }

                return new AnySelector($selectors);
            }
        });
    }
}
