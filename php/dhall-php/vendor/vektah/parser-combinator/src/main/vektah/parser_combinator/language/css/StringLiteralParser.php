<?php


namespace vektah\parser_combinator\language\css;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\parser\Parser;
use vektah\parser_combinator\parser\PositiveMatch;

class StringLiteralParser extends Parser
{
    public function __construct()
    {
        $this->root = new Closure(new Choice(
            new Sequence('"', PositiveMatch::instance(), new Concatenate(new Many('[^\n\r\f"\\\\]+', '\\\\[nrf"]')), '"'),
            new Sequence("'", PositiveMatch::instance(), new Concatenate(new Many("[^\n\r\f'\\\\]+", "\\\\[nrf']")), "'")
        ), function($data) {
            return $data[1];
        });
    }

    /**
     * Parse the given input
     *
     * @param Input $input
     *
     * @return Result result
     */
    public function parse(Input $input)
    {
        return $this->root->parse($input);
    }
}
