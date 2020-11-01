<?php

namespace vektah\parser_combinator\language\css;

use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\OptionalChoice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\formatter\Ignore;
use vektah\parser_combinator\language\Grammar;
use vektah\parser_combinator\parser\RegexParser;
use vektah\parser_combinator\parser\RepSep;
use vektah\parser_combinator\parser\SingletonTrait;
use vektah\parser_combinator\parser\WhitespaceParser;

/**
 * Parser for CSS files.
 */
class CssParser extends Grammar
{
    use SingletonTrait;

    public function __construct()
    {
        $this->selector = CssSelectorParser::instance();

        // Imported rules
        $this->ws = $this->selector->ws;
        $this->ident = $this->selector->ident;
        $this->string = $this->selector->string;
        $this->num = $this->selector->num;
        $this->dimension = $this->selector->dimension;

        $this->positive = $this->selector->positive;

        $escape = '\\\\[0-9a-fA-F]{1,6}|\\\\[^0-9a-fA-F\\r\\n]+';
        $nonascii = '[^\0-\177]';

        $this->hash = new Concatenate(new Sequence('#', "([a-zA-Z0-9_-]+|$escape|$nonascii)+"));

        $this->numeric = new Concatenate(new Sequence($this->num, new OptionalChoice('%', $this->ident)));

        $this->comment = new Ignore(new RegexParser('/\*.*?\*/\s*', 'ms'));

        $this->values = new Many($this->comment, $this->string, $this->hash, $this->ident, $this->numeric, ',', new WhitespaceParser(1));

        $this->function = new Concatenate(new Sequence(
            $this->ident,
            '(',
            $this->positive,
            $this->values,
            ')'
        ));

        $this->values->prepend($this->function);


        $this->declaration = new Closure(
            new Sequence(
                $this->ident,
                $this->ws,
                ':',
                $this->ws,
                $this->values,
                $this->ws,
                new OptionalChoice('!important')
            ),
            function($data) {
                return new CssDeclaration($data[0], implode(' ', $data[2]), $data[3] !== null);
            }
        );

        $this->ruleset = new Closure(
            new Sequence(
                $this->selector,
                $this->positive,
                $this->ws,
                '{',
                $this->ws,
                new RepSep($this->declaration, ';\s*'),
                '}',
                $this->ws
            ),
            function($data) {
                return new CssRuleSet($data[0], $data[2]);
            }
        );

        $this->root = new Closure(new Many($this->ruleset, $this->comment), function($data) {

            return new Css($data);
        });
    }

    /**
     * @param string $input
     * @return Css
     */
    public function parseString($input) {
        return parent::parseString($input);
    }
}
